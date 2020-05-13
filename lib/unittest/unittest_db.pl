:- module(unittest_db, [], [hiord, datafacts]).

% This module deals with everything related to the test database
% shared between the driver, runner and other parts of unittest. It
% defines the relevant data as well as the predicates to read/write it
% from/to files for sharing with the runner or for regression.

% Important pieces of data:

  % List of modules under test

  % List of tests and tests attributes

  % List of tests results

% This data needs to be shared and stored as files for the following:

  % sharing between driver and test runner

    % The test driver reads and asserts the test attributes, but the
    % test runner, which runs in another process, also needs them.

    % The test runner produces the test results, but the test driver
    % is the one that shows and stores them

  % saving it for regression, so we can compare saved results against
  % new results

% As a result, we have the following data:

  % module_base_path_db/3: The modules being tested, with their base
  % and absolute paths

  % test_attributes_db/n: The tests and their attributes for the
  % current unittest run. It might be a subset of all the tests
  % available in the modules under test, when filter options are used.

  % test_output_db/2: The result for each solution generated for the
  % goals under test

  % test_output_error/3: The standard output and error for each test

% Which are shared in the following files:

  % module.testout: File that stores test results

  % module.testout-saved: File that stores saved test results for
  % regression

  % module.testin: File that saves the test attributes. It always
  % contains all the tests in a module, even if filter options are
  % used.

  % module.testin-saved: Saved version of .testin file for regression.

  % <tmp_dir>/test_input_auto: shares test attributes between driver
  % and runner

  % <tmp_dir>/test_output_oauto: shares test results between runner
  % and driver

:- use_module(library(streams), [open/3, close/1, absolute_file_name/7]).
:- use_module(library(pathnames), [path_split/3]).
:- use_module(library(compiler/c_itf), [defines_module/2]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(stream_utils), [string_to_file/2]).

:- use_module(library(unittest/unittest_base), [write_data/2]).
:- use_module(library(unittest/unittest_utils), [assert_from_file/2]).

%%%%%%%%%%%%%%%%%%%%
%%%%% database %%%%%
%%%%%%%%%%%%%%%%%%%%

% -----------------------------
% modules under test
% -----------------------------

:- export(module_base_path_db/3).
:- data module_base_path_db/3.

:- export(assert_module_under_test/1).
assert_module_under_test(Module) :- % arg is a module
    defines_module(Base, Module), !,
    atom_concat(Base,'.pl',AbsFile),
    assertz_fact(module_base_path_db(Module, Base, AbsFile)).
% Note: the predicate is called this way only after a
% get_code_and_related_assertions and for related modules, so
% defines_module/2 will work
%
assert_module_under_test(Alias) :- % arg is an alias path
    absolute_file_name(Alias, '_opt', '.pl', '.', AbsFileName, Base, AbsDir), !,
    path_split(Base, AbsDir, Module),
    assertz_fact(module_base_path_db(Module, Base, AbsFileName)).
% TODO: check FileName exists, just in case

:- export(cleanup_modules_under_test/0).
cleanup_modules_under_test :-
    retractall_fact(module_base_path_db(_,_,_)).

% -----------------------------
% test attributes
% -----------------------------

:- export(test_attributes_db/8).
:- data test_attributes_db/8.

:- export(cleanup_test_attributes/0).
cleanup_test_attributes :-
    retractall_fact(test_attributes_db(_, _, _, _, _, _, _, _)).

:- export(assert_test_attributes/1).
assert_test_attributes(test_attributes_db(A, B, C, D, E, F, G, H)) :-
    assertz_fact(test_attributes_db(A, B, C, D, E, F, G, H)).

:- export(filter_test_attributes_db/1).
:- meta_predicate filter_test_attributes_db(pred(1)).
filter_test_attributes_db(Filter) :-
    ( % (failure-driven loop)
        test_attributes_db(A, B, C, D, E, F, G, H),
          \+ (Filter(test_attributes_db(A, B, C, D, E, F, G, H))),
          retract_fact(test_attributes_db(A, B, C, D, E, F, G, H)),
        fail
    ;
        true
    ).

% -----------------------------
% test results
% -----------------------------

:- export(test_output_db/2).
:- data test_output_db/2.

:- export(test_output_error_db/3).
:- data test_output_error_db/3.

assert_test_results(test_output_db(A, B)) :-
    assertz_fact(test_output_db(A, B)).
assert_test_results(test_output_error_db(A, B, C)) :-
    assertz_fact(test_output_error_db(A, B, C)).

:- export(cleanup_test_results/0).
cleanup_test_results :-
    retractall_fact(test_output_db(_, _)),
    retractall_fact(test_output_error_db(_, _, _)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% data to files and files to data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: runtest_input and test_input are too similar, rename one of
% them


:- meta_predicate data_to_file(addterm(goal),?,?).
% TODO: unify serialization in Ciao
data_to_file(Data, Term, File, Mode) :-
    open(File, Mode, Stream),
    ( % (failure-driven loop)
        current_fact(Data), % TODO: use a get_data/1 predicate, with one clause for each data
          write_data(Stream, Term),
        fail
    ;
        close(Stream)
    ).


% ------------------------------------
% .testin
% ------------------------------------

file_test_input_suffix('.testin').

:- export(file_test_input/2).
file_test_input(Module, TestIn) :-
    file_test_input_suffix(TestInSuffix),
    module_base_path_db(Module,Base,_),
    atom_concat(Base,TestInSuffix,TestIn).

:- export(test_input_file_to_test_attributes_db/1).
test_input_file_to_test_attributes_db(Module) :-
    file_test_input(Module,TestIn),
    assert_from_file(TestIn, assert_test_attributes).

% ------------------------------------
% .testin-saved
% ------------------------------------

file_saved_test_input_suffix('.testin-saved').

:- export(file_saved_test_input/2).
file_saved_test_input(Module, TestIn) :-
    file_saved_test_input_suffix(TestInSuffix),
    module_base_path_db(Module,Base,_),
    atom_concat(Base,TestInSuffix,TestIn).

:- export(saved_test_input_file_to_test_attributes_db/1).
saved_test_input_file_to_test_attributes_db(Module) :-
    file_saved_test_input(Module,TestIn),
    assert_from_file(TestIn, assert_test_attributes).

% -------------------------------------
% .testout
% -------------------------------------

file_test_output_suffix('.testout').

:- export(file_test_output/2).
file_test_output(Module, TestOut) :-
    file_test_output_suffix(TestOutSuffix),
    module_base_path_db(Module,Base,_),
    atom_concat(Base,TestOutSuffix,TestOut).

:- export(file_test_output_to_test_results_db/1).
file_test_output_to_test_results_db(Module) :-
    file_test_output(Module,TestOut),
    assert_from_file(TestOut, assert_test_results).

:- export(test_results_db_to_file_test_output/1).
test_results_db_to_file_test_output(Module) :-
    file_test_output(Module,TestOut),
    string_to_file("", TestOut),
    (  % (failure-driven loop)
        test_attributes_db(TestId,Module,_,_,_,_,_,_),
          data_to_file(test_output_db(TestId,_),TestOut, append),
          data_to_file(test_output_error_db(TestId,_,_),TestOut, append),
        fail
    ;
        true
    ).
% TODO: keep previous results when running this with filtered tests
% (if previous results were up to date)

% -------------------------------------
% .testout-saved
% -------------------------------------

file_saved_test_output_suffix('.testout-saved').

:- export(file_saved_test_output/2).
file_saved_test_output(Module, TestOut) :-
    file_saved_test_output_suffix(TestOutSuffix),
    module_base_path_db(Module,Base,_),
    atom_concat(Base,TestOutSuffix,TestOut).

:- export(file_saved_test_output_to_test_results_db/1).
file_saved_test_output_to_test_results_db(Module) :-
    file_saved_test_output(Module,TestOut),
    assert_from_file(TestOut, assert_test_results).

% -------------------------------------
% runtest input (file from passing test inputs from driver to runner)
% -------------------------------------


file_runtest_input_name('test_input_auto.pl').

:- export(file_runtest_input/2).
file_runtest_input(TestRunDir, InFile) :-
    file_runtest_input_name(BFileTestInput),
    path_concat(TestRunDir, BFileTestInput, InFile).

:- export(runtest_input_file_to_test_attributes_db/1).
runtest_input_file_to_test_attributes_db(TestRunDir) :-
    file_runtest_input(TestRunDir, FileTestInput),
    assert_from_file(FileTestInput, assert_test_attributes).

:- export(test_attributes_db_to_runtest_input_file/1).
test_attributes_db_to_runtest_input_file(TestRunDir) :-
    file_runtest_input(TestRunDir, FileTestInput),
    data_to_file(test_attributes_db(_,_,_,_,_,_,_,_), FileTestInput, write).    

% -------------------------------------
% runtest output (file for passing test outputs from runner to driver)
% -------------------------------------

file_runtest_output_name('test_output_auto.pl').

:- export(file_runtest_output/2).
file_runtest_output(TestRunDir, TestOut) :-
    file_runtest_output_name(BFileTestOutput),
    path_concat(TestRunDir, BFileTestOutput, TestOut).    

:- export(runtest_output_file_to_test_results_db/1).
runtest_output_file_to_test_results_db(TestRunDir) :-
    file_runtest_output(TestRunDir, TestOut),
    assert_from_file(TestOut, assert_test_results).
