:- module(_,[main/1],[]).

:- use_module(library(compiler), [use_module/2]).
:- use_module(library(pathnames), [path_concat/3]).


:- use_module(library(unittest/unittest_runner_aux),
              [
                  process_runner_args/1,
                  get_active_test/2
              ]).
:- use_module(library(unittest/unittest_utils), [assert_from_file/2]).
:- use_module(library(unittest/unittest_base),
              [
                  file_test_input/1,
                  file_test_output/1,
                  write_data/2,
                  get_stdout_redirection_file/2,
                  get_stderr_redirection_file/2
              ]).
:- use_module(library(unittest/unittest_db),
              [
                  assert_test_attributes/1
              ]).

% stop_on_first_error(false).
main(Args0) :-
    import_modules(Args0,[TestRunDir|Args]),
    process_runner_args(Args),
    file_test_input(InFile),
    path_concat(TestRunDir, InFile, FileTestInput),
    assert_from_file(FileTestInput,assert_test_attributes),
    file_test_output(OutFile),
    path_concat(TestRunDir, OutFile, FileTestOutput),
    runtests(TestRunDir, FileTestOutput).

import_modules(['--end_wrapper_modules--'|Args], Args) :- !.
import_modules([M|Ms], Args) :-
    intercept(
        use_module(M,[]), % we only care about multifile internal_runtest_module/2.
        compilation_error,
        halt(101) % return code handled by unittest.pl
    ),
    import_modules(Ms, Args).
% TODO: distinguish which is the module that does not compile

runtests(TestRunDir, OutputFile) :-
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
        runtest_module(TestRunDir, OutputFile, Module, TestId),
        fail
    ; true
    ).

:- use_module(library(streams), [open/3, close/1, current_output/1, set_output/1]).
:- use_module(library(stream_utils), [file_to_string/2]).
:- use_module(library(io_alias_redirection), [set_stream/3]).

runtest_module(TestRunDir, TestOutputFile, Module, TestId) :-
    get_stdout_redirection_file(TestRunDir,  StdoutFile),
    open(StdoutFile, write, OutputStream),
    get_stderr_redirection_file(TestRunDir, StderrFile),
    open(StderrFile, write, ErrorStream),
    current_output(CO),
    set_output(OutputStream),
    set_stream(user_error,ErrorStream,OldError),
    (internal_runtest_module(Module, TestId), fail ; true),
    set_stream(user_error,OldError, ErrorStream),
    set_output(CO),
    close(OutputStream),
    close(ErrorStream),
    file_to_string(StdoutFile, StdoutString),
    file_to_string(StderrFile, StderrString),
    open(TestOutputFile, append, S),
    write_data(S, test_output_error_db(TestId, StdoutString, StderrString)),
    close(S).
% TODO: unify with io_port_reify or something similar which provides
% an interface similar to process_call for output and error
% redirection

:- multifile internal_runtest_module/2.
% would it be better the following lines when calling internal_runtest_module/2?
%% atom_concat(Module,'_wrp_auto',WrpModule),
%% Goal = WrpModule:internal_runtest_module(Module, TestId),
%% call(Goal)
