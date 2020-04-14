:- module(_,[show_test_summaries/1],[assertions, nativeprops]).


:- doc(title,  "Testing summaries").

% copied from unittest.pl
:- doc(author, "Edison Mera").
:- doc(author, "Pedro L@'{o}pez").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Alvaro Sevilla San Mateo").
:- doc(author, "Nataliia Stulova").
:- doc(author, "Ignacio Casso").

:- doc(summary, "This module implements predicates for printing the
results of the tests.").

:- doc(stability,beta).

:- doc(module, "This module implements predicates for printing the
results of the tests. Each test has a result for each solution
generated for the predicate under test. The results can be one of the
followings:

@begin{itemize}

@item Test passed succesfully

@item Test failed

@item Test passed with warnings

  @begin{itemize}

  @item Predicate under test failed but that was allowed by the test assertions

  @item Predicate under test throwed an exception but that was allowed
  by the test assertions

  @item No test instance was generated from the calls field of the assertion

  @end{itemize}

@item Test aborted

  @begin{itemize}

  @item Test aborted for unmonitored reasons

  @item Test timed out

  @item Generation from calls field throwed an exception

  @item Postcondition-checking throwed an exception

@end{itemize}


The output for the test is divided in a one-line summary of the test
results plus an optional detailed output for each test solution. In
the presence of multiple solutions, multiple tests instances from the
calls field of the test assertion, or running the test multiple times,
the one-line summary of the test will be the result with the higher
priority, according to this list: first, test aborted; second, test
failed; third, test passed with warnings, and finally, test passed
successfully.

For the detailed output of each solution, the result, runtime-checks
and signals intercepted are printed.

").



:- use_module(library(llists), [flatten/2]).
:- use_module(library(hiordlib), [foldl/4]).
:- use_module(engine(messages_basic), [messages/1]).
:- use_module(library(lists), [append/3, member/2, reverse/2]).
:- use_module(library(terms), [atom_concat/2]).

:- use_module(library(rtchecks/rtchecks_pretty),
    [
        pretty_prop/3,
        rtcheck_to_messages/2
    ]).

% --------------------------------------------------------------

:- pred show_test_summaries(TestSummaries) : list(list(test_summary),TestSummaries) % one list per module
# "Pretty print the test results contained in @var{TestSummaries}.".

:- prop test_summary(X).

test_summary(TestAttributes-TestResults) :-
    test_attributes(TestAttributes),
    list(test_result, TestResults).

test_attributes(test_attributes(_Module, _F, _A, _Dict, _Comment,
                                _Source, _LB, _LE)).

test_result(st(ResultId, RtcErrors, Signal, Result)) :-
    result_id(ResultId),
    list(RtcErrors), % list(rtc_error,RtcErrors), rtc_error should be defined somewhere
    list(Signal),
    result(Result).

result_id(result_id(Time,Case,Sol)) :-
    int(Time),
    int(Case),
    int(Sol).

result(true).
result(fail(predicate)).
result(exception(predicate,_)).
result(fail(precondition)).
result(exception(precondition,_)).
result(exception(postcondition,_)).
result(timeout).
%
% introduced by unittest.pl when no output was saved by the test
% runner. Output and Error are the runner standard and error
% outputs. Bug: If the test runner had time to write some output
% before aborting (e.g., it aborted in the second solution of the
% predicate), the information that it aborted is lost
result(aborted(Output, Error)) :-
    string(Output),
    string(Error).
% TODO: complete

% -------------------------------------------------------------------


show_test_summaries(IdxTestSummaries0) :-
    % TODO: multiple test results bug
    flatten(IdxTestSummaries0, IdxTestSummaries),
    tests_results_to_messages(IdxTestSummaries, Messages, []),
    % TODO: rtchecks_pretty:compact_list/2 was called here, needed?
    messages(Messages).

tests_results_to_messages([], Messages, Messages).
tests_results_to_messages([TestAttributes-TestResults|IdxTestSummaries], Messages, MessagesTail) :-
    one_test_results_to_messages(TestAttributes, TestResults, Messages, Messages0),
    tests_results_to_messages(IdxTestSummaries, Messages0, MessagesTail).


one_test_results_to_messages(TestAttributes, TestResults, Messages, MessagesTail) :-
    one_line_message(TestAttributes, TestResults, Messages, Messages0),
    % TODO: next line only if verbose
    one_test_results_to_messages_(TestResults, TestAttributes, Messages0, MessagesTail).

one_test_results_to_messages_([], _, Messages, Messages).
one_test_results_to_messages_([TestResult|TestResults], TestAttributes, Messages, MessagesTail) :-
    one_result_to_messages(TestResult, TestAttributes, Messages, Messages0),
    one_test_results_to_messages_(TestResults, TestAttributes, Messages0, MessagesTail).

one_line_message(TestAttributes, TestResults, [Msg|Messages], Messages) :-
    one_line_message_(TestAttributes, TestResults, Msg), !.
one_line_message(_, _, X, X).

one_line_message_(TestAttributes, TestResults, Msg) :- % test aborted
    first_result(TestResults, aborted(_,_)), !,
    header(aborted,TestAttributes,Msg,Description),
    Description = [' Predicate under test aborted.'].
%
one_line_message_(TestAttributes, TestResults, Msg) :- % geneneration from calls field failed
    first_result(TestResults, fail(precondition)), !,
    header(warning,TestAttributes,Msg,Description),
    Description = [' Nothing tested because generation from calls field failed.'].
%
one_line_message_(TestAttributes, TestResults, Msg) :- % geneneration from calls field throwed an exception
    first_result(TestResults, exception(precondition,_)), !,
    header(aborted,TestAttributes,Msg,Description),
    Description = [' Nothing tested because generation from calls field aborted.'].
%
one_line_message_(TestAttributes, TestResults, Msg) :- % postcondition checking throwed an exception
    first_result(TestResults, exception(postcondition,_)), !,
    header(aborted,TestAttributes,Msg,Description),
    Description = [' Exception thrown while checking test success field.'].
%
one_line_message_(TestAttributes, TestResults, Msg) :- % test timed out
    first_result(TestResults, timeout), !,
    header(aborted,TestAttributes,Msg,Description),
    Description = [' Time limit for the test exceeded.'].
%
one_line_message_(TestAttributes, TestResults, Msg) :- % there were runtime-checks
    Status = st(_,[_|_],_,_),
    member(Status, TestResults), !,
    header(failed,TestAttributes,Msg,[]).
%
one_line_message_(TestAttributes, TestResults, Msg) :- % predicate under test throwed an exception
    first_result(TestResults, exception(predicate, _)), !,
    header(warning,TestAttributes,Msg,Description),
    Description = [' There were exceptions, but test does not specify exceptions behavior.'].
% TODO: do not print this if the test did specify exceptions behaviour. Include that information in TestAttributes
%
one_line_message_(TestAttributes, TestResults, Msg) :- % predicate under test failed
    first_result(TestResults, fail(predicate)), !,
    header(warning,TestAttributes,Msg,Description),
    Description = [' Goal tested failed, but test does not specify failure behavior.'].
% TODO: do not print this if the test did specify failure behaviour. Include that information in TestAttributes
%
one_line_message_(TestAttributes, _TestResults, Msg) :- % everything ok
    header(passed,TestAttributes,Msg,[]).

first_result(TestResults, Result) :-
    Status = st(_,_,_,Result),
    member(Status, TestResults), !.


header(Status, TestAttributes, Msg, MsgTail) :-
    TestAttributes = test_attributes(Module, F, A, _Dict, Comment, Source, LB, LE),
    status_text(Status,StatusMsg),
    module_text(Source,Module,ModuleMsg),
    descriptor_text(Comment, CommentMsg),
    Msg = message_lns(Source, LB, LE, note, [StatusMsg, ': ', ModuleMsg, F,
                    '/', A, [](CommentMsg), '.'|MsgTail]).
% TODO: get rid of 'Note: ', but still use messages and lines in the
% correct format.


status_text(passed,'PASSED').
status_text(failed,'FAILED').
status_text(warning,'WARNING').
status_text(aborted,'ABORTED').

module_text(Source,Module,'') :-
    atom_concat([_,'/',Module,'.pl'],Source), !.
module_text(_Source,Module,Text) :- % TODO: ignore it anyway?
    atom_concat(Module,':',Text).

descriptor_text("",      '') :- !.
descriptor_text(Comment, [' "', $$(Comment), '"']).


% -----------------------------------------------------------------

one_result_to_messages(Status, TestAttributes, Messages, MessagesTail) :-
    TestAttributes = test_attributes(_Module, _F, _A, Dict, _Comment,
                                     Source, LB, LE),
    Status = st(ResultId,RTCErrors, Signals0, Result0),
    pretty_prop(t(Result0, Signals0), Dict, t(Result, Signals)),
    specific_result_message(Result, Msg, SignalsMsg),
    signals_text(Signals, SignalsMsg, []),
    foldl(rtcheck_to_messages_, RTCErrors, Messages0, MessagesTail),
    ( Msg==[], Messages0==MessagesTail -> % nothing to write
        Messages=MessagesTail
    ;
        (ResultId = result_id(Time,Case,Sol) ->
            HeaderMsg = ['Time ', Time, ', case ', Case, ', solution ', Sol, ':\n' | Msg]
        ; HeaderMsg = Msg),
        Messages = [message_lns(Source, LB, LE, note, HeaderMsg) | Messages0]
    ),
    !.


% Note that the stronger result has already been output in the
% one-line summary, where (aborted > fail(precond) = ex(preocond) >
% ex(poscond) > timeout > rtcheck > ex(pred) > fail(pred) > true)
specific_result_message(true, Msg, Msg). % TODO:
specific_result_message(rtcheck_error, Msg, Msg). % TODO:
specific_result_message(fail(predicate), Msg, Msg). % TODO:
specific_result_message(exception(predicate,_), Msg, Msg). % TODO:
specific_result_message(fail(precondition), Msg, Msg). % TODO:
specific_result_message(exception(precondition,_), Msg, Msg). % TODO:
specific_result_message(exception(postcondition,_), Msg, Msg). % TODO:
specific_result_message(timeout, Msg, Msg). % TODO:
specific_result_message(aborted(_Output, []), Msg, Tail) :- !,
    Msg = ['Test aborted.\n'| Tail].
specific_result_message(aborted(_Output, Error), Msg, Tail) :-
    atom_codes(ErrorAtom, Error),
    Msg = ['Test aborted with following message in standard error:\n', ErrorAtom | Tail].


signals_text([], Msg, Msg) :- !.
signals_text(Signals, [' Signals thrown: ', ~~(Signals) | Msg], Msg).

rtcheck_to_messages_(E, Msgs, Msgs0) :-
    rtcheck_to_messages(E, Ys),
    append(Ys, Msgs0, Msgs).

% TODO: unify with unittest_statistics
