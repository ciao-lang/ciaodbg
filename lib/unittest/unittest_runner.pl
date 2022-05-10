:- module(_,[main/1],[]).

:- use_module(library(compiler), [use_module/2]).
:- use_module(library(lists), [member/2]).
:- use_module(library(io_port_reify), [open_std_redirect/3, close_std_redirect/1]).
:- use_module(library(streams), [open/3, close/1]).
:- use_module(library(assertions/assrt_lib), [assertion_body/7]).

:- use_module(library(unittest/unittest_runner_aux), [
    process_runner_args/4,
    get_active_test/2,
    testing/5,
    timed_out/0
]).
:- use_module(library(unittest/unittest_base), [write_data/2]).
:- use_module(library(unittest/unittest_base), [
    test_redirect_mode/3,
    test_redirect_contents/4,
    test_redirect_chn/4
]).
:- use_module(library(unittest/unittest_db), [
    runtest_input_file_to_test_attributes_db/1,
    file_runtest_output/2,
    test_attributes_db/8
]).

% stop_on_first_error(false).

% Expected arguments in main/1:
%   --begin_module_wrappers-- Mods --end_module_wappers--: List of Wrappers for modules under test.
%   dir TestRunDir: Temporary directory for test files.
%   timeout Timeout: Default timeout for tests.
%   resume_after ARef: Optional argument to skip tests until test with id ARef.
%   suff Suff. Optional argument to set optional suffix (internals:opt_suff/1) in runner
%   Stdout Mode: dump_output_real_time, ignore_output...
%   Stderr Mode: dump_error_real_time, ignore_error, error_to_output...
%   load Module: Additional module to load. Unused. Obsolete?
%   other unittest driver options (unittest:test_option/1) might be passed down to the runner aside from stdout and stderr options. They are ignored.
main(Args0) :-
    process_runner_args(Args0, TestRunDir, WrpModules, Args),
    import_modules(WrpModules),
    runtest_input_file_to_test_attributes_db(TestRunDir), % asserts test inputs as test_attributes_db/n
    file_runtest_output(TestRunDir, FileTestOutput),
    runtests(TestRunDir, FileTestOutput, Args).

import_modules([]).
import_modules([M|Ms]) :-
    intercept(
        use_module(M,[]), % we only care about multifiles test_check_pred/3 and test_entry/3
        compilation_error,
        halt(101) % return code handled by unittest.pl
    ),
    import_modules(Ms).
% TODO: distinguish which is the module that does not compile

runtests(TestRunDir, OutputFile, Args) :-
    ( % (failure-driven loop)
      get_active_test(TestId, Module),
      % TODO: use data predicate to store the testing
      %       status of the predicate, whether some
      %       input failed (thus no testing to be
      %       continued), or no
      % TODO: requires splitting runtests/0 into 2
      %       preds with 2 failure-driven loops,
      %       one for TestIds and another for all
      %       results for a chosen TestId
        runtest_module(TestRunDir, OutputFile, Args, Module, TestId),
        ( timed_out ->
            !, % (end loop)
            % previous test had a timeout, force a runner restart
            open(OutputFile, append, S),
            write_data(S, test_output_event(continue_after(TestId))),
            close(S)
        ; fail % (loop)
        )
    ; true
    ).

runtest_module(TestRunDir, TestOutputFile, Args, Module, TestId) :-
    test_redirect_mode(stdout, Args, OutputMode),
    test_redirect_mode(stderr, Args, ErrorMode),
    maybe_redirect(OutputMode, stdout, TestRunDir, OutputRedirect),
    maybe_redirect(ErrorMode, stderr, TestRunDir, ErrorRedirect),
    ( internal_runtest_module(Module, TestId, TestRunDir), fail ; true ),
    maybe_close_redirect(ErrorRedirect),
    maybe_close_redirect(OutputRedirect),
    test_redirect_contents(OutputMode, stdout, TestRunDir, Output),
    test_redirect_contents(ErrorMode, stderr, TestRunDir, Error),
    open(TestOutputFile, append, S),
    write_data(S, test_output_error_db(TestId, Output, Error)),
    close(S).

maybe_redirect(Mode, Std, TestRunDir, Redirect) :-
    test_redirect_chn(Mode, Std, TestRunDir, Chn),
    ( Chn = none -> Redirect = none
    ; open_std_redirect(Std, Chn, Redirect)
    ).

maybe_close_redirect(none) :- !.
maybe_close_redirect(Redirect) :- close_std_redirect(Redirect).

:- multifile test_entry/3.
:- multifile test_check_pred/3.
% would it be better the following lines when calling test_check_pred/2?
%% atom_concat(Module,'_wrp_auto',WrpModule),
%% Goal = WrpModule:test_check_pred(Module, TestId),
%% call(Goal)

% TODO: temporary, pending unification with unittest.pl
internal_runtest_module(Module, TestId, TestRunDir) :-
    test_attributes_db(TestId, Module, _, _, _, Options, Body, _),
    assertion_body(Pred,_,_,_,_,_,Body),
    testing(TestId, TestRunDir,
            test_entry(Module,TestId,Pred), % calls field of Pred
            test_check_pred(Module, TestId, Pred), % rtcheck version of Pred
            Options).

