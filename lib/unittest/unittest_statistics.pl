:- module(unittest_statistics,
        [
            statistical_summary/2, 
            statistical_filter/3
        ],
        [assertions]).

:- use_module(engine(messages_basic), [display_list/1]).
:- use_module(engine(messages_basic), [message/2]).
:- use_module(library(lists),  [length/2]).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(format), [sformat/3]).

:- doc(title,  "Testing statistics").
:- doc(author, "Alvaro Sevilla San Mateo").

:- doc(stability,beta).

:- doc(module, "This module implements predicates for generating
       statistical summaries for the testing processes.").

:- pred statistical_summary(Tag, IdxTestSummaries) : atm * list

# "Makes the statistic summary with the results of the
   tests. @var{IdxTestSummaries} contains a list of terms with the
   results of the tests.".

% The results of a test, written in an output file by the test runner,
% are a term of the form st(RtcErrors,Signals,Status) for each
% solution of the predicate, where:
%
% - RtcErrors is the list of runtime-check errors intercepted for that
%   solution
%
% - Signals is the list of other signals intercepted for that solution
%
% - Status is one of the following:
%
%   - aborted(?,?): The test aborted for some reason. This result is
%     added manually by unittest.pl when it finds no test results (so
%     it can not be a result of a second solution, and in particular
%     currently we don't know if a test aborted after the first
%     solution)
%
%   - fail(precondition): The precondition of the test assertion had
%     no solutions when being run, and therefore no actual goal to be
%     run for testing was generated.
%
%   - exception(Where,E): There was an exception at step Where of the
%     test. Where in {precondition, postcondition, predicate}
%
%   - fail(predicate): The predicate did not succeed for the given
%     test case
%
%   - true: The predicate succeeded for the given test case

% TODO: move this documentation to unittest.pl

% IdxTestSummaries is of the form
% [[TestAttributes-[count(st(RtcErrors,Signals,TestResult), N)]]],
% where N is the number of times the test result st(...) occurs for
% the test with TestAttributes
statistical_summary(Tag, IdxTestSummaries0) :-
    flatten(IdxTestSummaries0, IdxTestSummaries),
    statistical_filter(IdxTestSummaries, stats(0,0,0,0,0,0),
        stats(NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors)),
    NTotal is NSuccess+NFail+NFailPre+NAborted+NTimeout,
    NTotal > 0, !,
    sformat(S, "Passed: ~w (~2f\%) Failed: ~w (~2f\%) " ||
        "Precond Failed: ~w (~2f\%) Aborted: ~w (~2f\%) " ||
        "Timeouts: ~w (~2f\%) " ||
        "Total: ~w Run-Time Errors: ~w~n}~n",
        [
            NSuccess, 100*NSuccess/NTotal,
            NFail, 100*NFail/NTotal,
            NFailPre, 100*NFailPre/NTotal,
            NAborted, 100*NAborted/NTotal,
            NTimeout, 100*NTimeout/NTotal,
            NTotal,
            NRTCErrors
        ]),
    display_list(Tag),
    message(note, [$$(S)]).
statistical_summary(_,_). % reached if NTotal=0. Print something?

% TODO: define type stats(NSuccess, NFail, NFailPre, NAborted, NTimeout, NErrors), all ints

:- pred statistical_filter(IdxTestSummaries, Stats0, Stats)
%% Stats0 = stats(NSuccess0, NFail0, NFailPre0, NAborted0, NTimeout0, NErrors0)
%% Stats  = stats(NSuccess,  NFail,  NFailPre,  NAborted,  NTimeout,  NRTCErrors)

# "Narrow the information of the tests and generate the statistical
   information structure needed to perform the statistical summary.
   @var{IdxTestSummaries} contains a list of terms with the results of
   tests. ".

statistical_filter([], Stats, Stats).
statistical_filter([_-TestSummary|TSs], Stats0, Stats) :-
    update_summary(TestSummary, Stats0, Stats1),
    statistical_filter(TSs, Stats1, Stats).

% case 1: There was no output written by the tests, so unittest.pl
% deduces the test aborted and writes itself the output st(_,_,aborted(?,?)).
update_summary([count(st(_, _, aborted(_, _)),_C)|_Rest], Stats0, Stats) :- !, % _C=1, _Rest=[]
    inc_stat(aborted,Stats0,Stats).
%
% case 2: The precondition of the test failed
update_summary([count(st(_, _, fail(precondition)),_C)|_Rest], Stats0, Stats) :- !, % _C=1, _Rest=[]
    inc_stat(fail_pre,Stats0,Stats).
%
% case 3: The precondition of the test threw an exception
update_summary([count(st(_, _, exception(precondition,_)),_C)|_Rest], Stats0, Stats) :- !, % _C=1, _Rest=[]
    inc_stat(aborted,Stats0,Stats). % or increase_fail_precondition?
%
% case 4: The postcondition of the test threw an exception
update_summary(Summ, Stats0, Stats) :-
    member(count(st(_, _, exception(postcondition,_))),Summ), !,
    inc_stat(aborted,Stats0,Stats).
%
% case 5: Catch timeout exceptions as a special case
% TODO: needs to be improved
update_summary([count(st(_, _, exception(predicate,timeout)),_C)|_Rest], Stats0, Stats) :- !, % ? 
    inc_stat(timeout,Stats0,Stats). 
%
% case 6: At least one runtime-check error occurred during testing
update_summary(Summ, Stats0, Stats) :-
    number_rtc_errors(Summ,N),
    N > 0,
    !,
    inc_stat(failed,Stats0,Stats1),
    inc_stat_n(rtchecks,Stats1,N,Stats).
%
% other cases: st([],_,St), St in {true, fail(predicate), exception(predicate)}
update_summary(_, Stats0, Stats) :-
    inc_stat(success,Stats0,Stats).

number_rtc_errors(Summ,N) :-
    number_rtc_errors_(Summ,0,N).

number_rtc_errors_([],N,N).
number_rtc_errors_([count(st(RTCErrors, _, _),C)|Summ],Acc0,N) :-
    length(RTCErrors,K),
    Acc is Acc0 + K*C,
    number_rtc_errors_(Summ,Acc,N).

inc_stat(Stat, Stats0, NewStats) :-
    inc_stat_n(Stat, Stats0, 1, NewStats).

inc_stat_n(success, stats(NSuccess0,NFail,NFailPre,NAborted,NTimeout,NRTCErrors), N,
           stats(NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors)) :-
    NSuccess is NSuccess0+N.
inc_stat_n(failed, stats(NSuccess,NFail0,NFailPre,NAborted,NTimeout,NRTCErrors), N,
           stats(NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors)) :-
    NFail is NFail0+N.
inc_stat_n(fail_pre, stats(NSuccess,NFail,NFailPre0,NAborted,NTimeout,NRTCErrors), N,
           stats(NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors)) :-
    NFailPre is NFailPre0+N.
inc_stat_n(aborted, stats(NSuccess,NFail,NFailPre,NAborted0,NTimeout,NRTCErrors), N,
           stats(NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors)) :-
    NAborted is NAborted0+N.
inc_stat_n(timeout, stats(NSuccess,NFail,NFailPre,NAborted,NTimeout0,NRTCErrors), N,
           stats(NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors)) :-
    NTimeout is NTimeout0+N.
inc_stat_n(rtchecks, stats(NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors0), N,
           stats(NSuccess,NFail,NFailPre,NAborted,NTimeout,NRTCErrors)) :-
    NRTCErrors is NRTCErrors0+N.


% TODO: IC: I have kept the original stats, but we should redesign
% which ones to output (e.g., precondition_failed probably is not that
% relevant, and should not even occur, better to just warn when it
% happens).
%
% MH: Actually, it indeed is something that the user does
% not intend but if we do not detect it and report it it is hard to
% spot these errors: they show up as tests that succeed even if they
% have not even run.
