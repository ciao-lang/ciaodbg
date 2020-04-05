:- module(_,
          [
              % data
              module_base_path_db/3,
              % preds
              assert_module_under_test/1,
              clean_db/0
          ],
          [datafacts]).

:- use_module(engine(stream_basic), [absolute_file_name/7]).
:- use_module(library(pathnames), [path_split/3]).
:- use_module(library(compiler/c_itf), [defines_module/2]).


:- use_module(library(unittest), [
    test_output_db/2,
    test_attributes_db/9,
    test_output_error_db/3
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% modules under test %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- data module_base_path_db/3.

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

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% output %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%


clean_output_db :-
    retractall_fact(test_output_db(_,_)),
    retractall_fact(test_attributes_db(_,_,_,_,_,_,_,_,_)),
    retractall_fact(test_output_error_db(_,_,_)).


%%%%%%%%%%%%%%%%%%%%%%

clean_db :-
    retractall_fact(module_base_path_db(_,_,_)),
    clean_output_db.

% TODO: move here all file and data related predicates from unittest.pl
