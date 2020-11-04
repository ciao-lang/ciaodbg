:- module(_,[main/1],[]).

:- use_module(library(compiler), [use_module/2]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(system), [mktemp_in_tmp/2]).
:- use_module(library(lists), [member/2]).
:- use_module(library(io_port_reify), [open_std_redirect/3, close_std_redirect/1]).
:- use_module(library(streams), [open/3, close/1]).
:- use_module(library(stream_utils), [file_to_string/2, write_string/1]).
:- use_module(library(assertions/assrt_lib), [assertion_body/7]).

:- use_module(library(unittest/unittest_runner_aux),
              [
                  process_runner_args/4,
                  get_active_test/2,
                  testing/5
              ]).
:- use_module(library(unittest/unittest_base),
              [
                  write_data/2,
                  get_stdout_redirection_file/2,
                  get_stderr_redirection_file/2
              ]).
:- use_module(library(unittest/unittest_db),
              [
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
        fail
    ; true
    ).


runtest_module(TestRunDir, TestOutputFile, Args, Module, TestId) :-
    get_stdout_option(Args, OutputMode),
    get_stderr_option(Args, ErrorMode),
    redirect_stdout(OutputMode, TestRunDir, OutputRedirect),
    redirect_stderr(ErrorMode, TestRunDir, ErrorRedirect),
    (internal_runtest_module(Module, TestId, TestRunDir), fail ; true),
    close_stderr_redirection(ErrorMode, ErrorRedirect),
    close_stdout_redirection(OutputMode, OutputRedirect),
    get_stdout(OutputMode, TestRunDir, Output),
    get_stderr(ErrorMode, TestRunDir, Error),
    open(TestOutputFile, append, S),
    write_data(S, test_output_error_db(TestId, Output, Error)),
    close(S).

:- multifile test_entry/3.
:- multifile test_check_pred/3.
% would it be better the following lines when calling test_check_pred/2?
%% atom_concat(Module,'_wrp_auto',WrpModule),
%% Goal = WrpModule:test_check_pred(Module, TestId),
%% call(Goal)

% ---------------------------------------------
% output and error options
% ---------------------------------------------

get_stdout_option(Args, dump_stdout_rt) :-
    member(dump_output_real_time, Args), !.
get_stdout_option(Args, ignore_stdout) :-
    member(ignore_output, Args), !.
get_stdout_option(Args, dump_stdout) :-
    member(dump_output, Args), !.
get_stdout_option(_, save_stdout). % default


get_stderr_option(Args, dump_stderr_rt) :-
    member(dump_error_real_time, Args), !.
get_stderr_option(Args, ignore_stderr) :-
    member(ignore_error, Args), !.
get_stderr_option(Args, stderr_to_stdout) :-
    member(error_to_output, Args), !.
get_stderr_option(Args, dump_stderr) :-
    member(dump_error, Args), !.
get_stderr_option(_, save_stderr). % default


redirect_stdout(dump_stdout_rt,_,_).
redirect_stdout(save_stdout,TestRunDir,Redirect) :-
    get_stdout_redirection_file(TestRunDir, StdoutFile),
    open_std_redirect(stdout, file(StdoutFile), Redirect).
redirect_stdout(ignore_stdout,_,Redirect) :-
    mktemp_in_tmp('tmpXXXXXX', File),
    open_std_redirect(stdout, file(File), Redirect). % TODO: add null redirection to io_port_reify?
redirect_stdout(dump_stdout,Dir,Red) :-
    redirect_stdout(save_stdout,Dir,Red).

redirect_stderr(dump_stderr_rt,_,_).
redirect_stderr(save_stderr,TestRunDir,Redirect) :-
    get_stderr_redirection_file(TestRunDir, StderrFile),
    open_std_redirect(stderr, file(StderrFile), Redirect).
redirect_stderr(stderr_to_stdout,_,Redirect) :-
    open_std_redirect(stderr, stdout, Redirect).
redirect_stderr(ignore_stderr,_,Redirect) :-
    mktemp_in_tmp('tmpXXXXXX', File),
    open_std_redirect(stderr, file(File), Redirect).
redirect_stderr(dump_stderr,Dir,Red) :-
    redirect_stderr(save_stderr,Dir,Red).

close_stdout_redirection(dump_stdout_rt, _) :- !.
close_stdout_redirection(_, OutputRedirect) :-
    close_std_redirect(OutputRedirect).

close_stderr_redirection(dump_stderr_rt, _) :- !.
close_stderr_redirection(_, ErrorRedirect) :-
    close_std_redirect(ErrorRedirect).

get_stdout(dump_stdout_rt, _, dumped).
get_stdout(save_stdout, TestRunDir, OutputString) :-
    get_stdout_redirection_file(TestRunDir, StdoutFile),
    file_to_string(StdoutFile, OutputString).
get_stdout(ignore_stdout, _, ignored).
get_stdout(dump_stdout, TestRunDir, OutputString) :-
    get_stdout(save_stdout, TestRunDir, OutputString),
    write_string(OutputString).

get_stderr(dump_stderr_rt, _, dumped).
get_stderr(save_stderr, TestRunDir, ErrorString) :-
    get_stderr_redirection_file(TestRunDir, StderrFile),
    file_to_string(StderrFile, ErrorString).
get_stderr(stderr_to_stdout, _, redirected_to_stdout).
get_stderr(ignore_stderr, _, ignored).
get_stderr(dump_stderr, TestRunDir, ErrorString) :-
    get_stderr(save_stderr, TestRunDir, ErrorString),
    write_string(ErrorString).

:- export(get_stdout/3).
:- export(get_stderr/3).
:- export(get_stdout_option/2).
:- export(get_stderr_option/2).
% tmp, pending unification with unittest.pl
internal_runtest_module(Module, TestId, TestRunDir) :-
    test_attributes_db(TestId, Module, _, _, _, Options, Body, _),
    assertion_body(Pred,_,_,_,_,_,Body),
    testing(TestId, TestRunDir,
            test_entry(Module,TestId,Pred), % calls field of Pred
            test_check_pred(Module, TestId, Pred), % rtcheck version of Pred
            Options).
