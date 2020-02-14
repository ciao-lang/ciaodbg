:- module(_,[main/1],[]).

:- use_module(library(compiler), [use_module/2]).
                      
:- use_module(library(unittest/unittest_runner_aux),
              [
                  process_runner_args/1,
                  assert_test_id/1,
                  get_active_test/2
              ]).
:- use_module(library(unittest/unittest_utils), [assert_from_file/2]).
:- use_module(library(unittest/unittest_base), []). % TODO: needed?


% stop_on_first_error(false).
main(Args0) :- % TODO: does this work for args that are not atoms (i.e., list of atoms)?
    import_modules(Args0,[FileTestInput|Args]),
    process_runner_args(Args),
    assert_from_file(FileTestInput,assert_test_id),
    runtests.

import_modules(['--end_wrapper_modules--'|Args], Args) :- !.
import_modules([M|Ms], Args) :-
    use_module(M,[]), % we only care about multifile internal_runtest_module/1.
    import_modules(Ms, Args).

runtests :-
    ( % (failure-driven loop)
      get_active_test(TestId, Module),
      % TODO: multiple test results bug
      % TODO: use data predicate to store the testing
      %       status of the predicate, whether some
      %       input failed (thus no testing to be
      %       continued), or no
      % TODO: requires splitting runtests/0 into 2
      %       preds with 2 failure-driven loops,
      %       one for TestIds and another for all
      %       results for a chosen TestId
      internal_runtest_module(Module, TestId),
      fail
    ; true
    ).


:- multifile internal_runtest_module/2.
