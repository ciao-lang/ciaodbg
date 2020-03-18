:- module(unittest_runner_aux,
    [ assert_test_id/1,
      get_active_test/2,
      process_runner_args/1,
      testing/5
    ],
    [assertions, hiord, datafacts]).

:- doc(title,"Testing support lib (runner)").

:- doc(author, "Edison Mera").

% TODO: postcondition failure treating?
% TODO: how rtchecks deals with exceptions in pre/postconditins
% TODO: move (parts of) this lib into rtchecks?

% ----------------------------------------------------------------------
:- use_module(library(compiler),   [use_module/1]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(pathnames),  [path_concat/3]).
:- use_module(engine(stream_basic), [open/3, close/1]).
:- use_module(library(timeout), [call_with_time_limit/3]).
:- use_module(library(between), [between/3]).

:- use_module(library(unittest/unittest_base),
    [
        write_data/2,
        file_test_output/1
    ]).


% ----------------------------------------------------------------------

:- data rtcheck_db/1.

:- meta_predicate save_rtchecks(goal).

:- pred save_rtchecks/1 : callable # "Asserts in rtcheck_db/1 all the
    run-time check exceptions thrown by the goal.".

save_rtchecks(Goal) :-
    retractall_fact(rtcheck_db(_)),
    RTError = rtcheck(_Type, _Pred, _Dict, _Prop, _Valid, _Poss),
    intercept(Goal, RTError, assertz_fact(rtcheck_db(RTError))). % TODO: wrong?! abort on errors?

:- pred load_rtchecks/1 => list # "retract the
    rtcheck_db/1 facts and return them in a list.".

load_rtchecks(RTChecks) :-
    findall(RTCheck, retract_fact(rtcheck_db(RTCheck)), RTChecks).

% ----------------------------------------------------------------------

:- data test_id_db/2.
:- data skip_tests_before/1.
:- data active_test/0.
:- data default_timeout/1.

% ----------------------------------------------------------------------

assert_test_id(test_input_db(ARef, Mod)) :-
    assertz_fact(test_id_db(ARef, Mod)).

get_active_test(ARef, Mod) :-
    (\+ skip_tests_before(_)), !,
     test_id_db(ARef, Mod).
get_active_test(ARef, Mod) :-
    retractall_fact(active_test),
    test_id_db(ARef0, Mod0),
    (  skip_tests_before(ARef0)
    -> assertz_fact(active_test),
       fail
    ; true ),
    active_test,
    ARef = ARef0,
    Mod  = Mod0.

process_runner_args([]) :- !.
process_runner_args([resume_after, ARef|Args]) :- !,
    retractall_fact(skip_tests_before(_)),
    assertz_fact(skip_tests_before(ARef)),
    process_runner_args(Args).
process_runner_args([load, Module|Args]) :- !,
    use_module(Module),
    process_runner_args(Args).
process_runner_args([timeout, TimeoutAtm|Args]) :-
    retractall_fact(default_timeout(_)),
    atom_number(TimeoutAtm,Timeout),
    assertz_fact(default_timeout(Timeout)),
    process_runner_args(Args).
process_runner_args([_|Args]) :-
    process_runner_args(Args).

% ----------------------------------------------------------------------

:- meta_predicate testing(?, ?, goal, goal, ?).

testing(ARef, TmpDir, Precond, Pred, Options) :-
    get_option(timeout,Options,Timeout),
    get_option(times,Options,NTimes),
    between(1,NTimes,_), % repeat n times
    testing_(Timeout,ARef, TmpDir, Precond, Pred, Options).
% TODO: how should output and statistics behave wrt times(N)?

:- meta_predicate testing_(?, ?, ?, goal, goal, ?).
testing_(none,ARef, TmpDir, Precond, Pred, Options) :- !,
    testing__(ARef, TmpDir, Precond, Pred, Options).
testing_(Timeout,ARef, TmpDir, Precond, Pred, Options) :-
    call_with_time_limit(
        Timeout, % mS
        testing__(ARef, TmpDir, Precond, Pred, Options),
        _ % time_limit_exception is caught and handled in an inner catch/3.
    ).

:- meta_predicate testing__(?, ?, goal, goal, ?).
testing__(ARef, TmpDir, Precond, Pred, Options) :-
    file_test_output(BOut),
    path_concat(TmpDir,BOut,Out),
    testing_internal(Precond, Pred, Options, Status),
      open(Out, append, IO),
      write_data(IO, test_output_db(ARef, Status)),
      close(IO),
    fail.
testing__(_,_,_,_,_).

:- data signals_db/1.

:- meta_predicate testing_internal(goal, goal, ?, ?).
testing_internal(Precond, Pred, Options, st(RTCErrors, Signals, Result)) :-
    retractall_fact(signals_db(_)),
    intercept(
        save_rtchecks(exec_test(Precond, Pred, Options, Result)),
        E,
        assertz_fact(signals_db(E))),
    findall(E, retract_fact(signals_db(E)), Signals),
    load_rtchecks(RTCErrors).

:- meta_predicate exec_test(goal, goal, ?, ?).
exec_test(Precond,Pred,Options,Result) :-
    generate_test_case(Precond,Options,Result),
    (nonvar(Result) -> true % some error in generation, returned as Result
    ;
        run_test(Pred,Options,Result)
    ).

:- meta_predicate generate_test_case(goal,?,?).
generate_test_case(Precond,Options,Result) :-
    get_option(generate_from_calls_n, Options, NCases),
    catch(
        backtrack_n_times(Precond,NCases,not_n_cases_reached(Result)),
        PrecEx,
        Result=exception(precondition, PrecEx)
    ).
% TODO: how should output and statistics behave wrt generate_from_calls_n(N)?

not_n_cases_reached(Result,0) :- !,
    Result = fail(precondition).
% not_n_cases_reached(Result,N) :- fail.

:- meta_predicate run_test(goal,?,?).
run_test(Pred,Options,Result) :-
    get_option(try_sols,Options,NSols),
    catch(
        backtrack_n_times((Pred, Result=true),NSols,not_n_sols_reached(Result)),
        Ex,
        run_test_exception(Ex,Result)
    ).
% TODO: coverage warnings when there are more than NSols solutions?

not_n_sols_reached(Result, 0) :- !,
    Result = fail(predicate).
% not_n_sols_reached(Result, N) :- fail.

run_test_exception(postcondition(PostEx),exception(postcondition,PostEx)) :- !.
run_test_exception(time_limit_exceeded,exception(predicate,timeout)) :- !.
run_test_exception(Ex,exception(predicate,Ex)).



get_option(Opt,Options,Value) :-
    functor(Option,Opt,2),
    member(Option,Options), !,
    arg(2,Option,Value).
get_option(times,_,1).
get_option(try_sols,_,2). % enough to capture determinism
get_option(timeout,_,Timeout) :-
    default_timeout(Timeout).
get_option(generate_from_calls_n,_,1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: move somewhere else
%
:- use_module(engine(internals),['$setarg'/4]).

% limit backtracking of Goal to N solutions, and optionally call
% Handler if there are less than N solutions

%% :- meta_predicate backtrack_n_times(goal,?).
%% backtrack_n_times(Goal,N) :-
%%     backtrack_n_times(Goal,N,fail).
%%
%% fail(_) :- fail.

:- meta_predicate backtrack_n_times(goal,?,pred(1)).
backtrack_n_times(Goal,N,Handler) :-
    N > 0,
    NSol = nsol(0),
    backtrack_n_times_(NSol,N,Goal,Handler).

:- meta_predicate backtrack_n_times(?, ?, goal, goal).
backtrack_n_times_(NSol,NSought,Goal,_) :-
    call(Goal),
    NSol=nsol(N),
    N1 is N+1,
    '$setarg'(1,NSol,N1,true),
    (NSol=nsol(NSought), ! ; true).
backtrack_n_times_(nsol(NFound),_,_,Handler) :-
    Handler(NFound).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: unify with ciaotest
