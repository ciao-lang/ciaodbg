:- module(_,
          [
              save_output/2,
              brief_compare/3,
              compare/2
          ],
          [hiord, datafacts]).

:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(engine(io_basic), [display/1, nl/0]).
:- use_module(library(write), [print/1]).
:- use_module(library(system), [copy_file/2]).
:- use_module(engine(stream_basic), [flush_output/0]).
:- use_module(library(stream_utils), [get_line/1, string_to_file/2]).
:- use_module(library(process), [process_call/3]).
:- use_module(engine(messages_basic), [message/2]).
:- use_module(library(system), [delete_file/1, get_tmp_dir/1]).
:- use_module(library(assertions/assrt_lib), [assertion_body/7]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(write), [numbervars/3]).

:- use_module(library(unittest/unittest_db)).

:- use_module(library(unittest/unittest_utils), [assert_from_file/2]).

%%%%% save

:- meta_predicate save_output(?, pred(1)).

% copied from octesting
save_output(Modules, Filter) :-
    Filter(anything), !, % Kludge
    input('Really save output for all modules? (yes/no/) ', Answer),
    ( Answer = "yes" ->
        save_output_(Modules),
        display('Saved'), nl
    ; display('Not saved'), nl
    ).
save_output(_, _) :-
    display('save action with filters not supported yet'), nl.
% TODO: options for asking for comfirmation once, once per module,
% once per test. Ask again if none of those


save_output_([]).
save_output_([M|Ms]) :-
    save_output__(M),
    save_output_(Ms).

save_output__(M) :-
    file_saved_test_output(M,SavedOutput),
    file_test_output(M,Output),
    del_file_nofail(SavedOutput),
    copy_file(Output, SavedOutput),
    file_saved_test_input(M,SavedInput),
    file_test_input(M,Input),
    del_file_nofail(SavedInput),
    copy_file(Input, SavedInput).

% TODO: warn and fail if .testout is incomplete becasue unittest was
% only run for a subset of the tests (using filter options). It is not
% trivial to merge .testout with .testoutsaved due to the fact that
% test ids depend on locators. The current solution we have for
% comparing them for regrassion could be used here, but it is a kludge
% right now

input(Question, Answer) :-
    display(Question), nl, flush_output,
    get_line(Answer).

%%%% brief compare

:- meta_predicate brief_compare(?, pred(1), ?).
brief_compare([], _, 0).
brief_compare([M|Ms], Filter, ReturnStatus) :-
    brief_compare_(M, Filter, MStatus),
    ( MStatus=1 -> ReturnStatus=1 ; ReturnStatus=ReturnStatus0 ),
    brief_compare(Ms, Filter, ReturnStatus0).

:- meta_predicate brief_compare_(?, pred(1), ?).
brief_compare_(M, Filter, 0) :-
    compare_tests(M, Filter, brief), !, % there are no differences
    display('Module '), display(M), display(': OK.'), nl.
brief_compare_(M, _, 1) :- % there are differences
    display('Module '), display(M), display(': There are differences.'), nl.


%%%% compare

compare([],_).
compare([M|Ms], Filter) :-
    compare_(M, Filter),
    compare(Ms, Filter).

compare_(M, Filter) :-
    display('Differences in module '), display(M), display(':'), nl,
    retractall_fact(there_were_differences),
    compare_tests(M, Filter, show), % TODO: better messages
    (there_were_differences -> true ; display('None'), nl).

:- data there_were_differences/0.

there_are_differences :-
    (there_were_differences -> true ; assertz_fact(there_were_differences)).

%%%%% compare tests

compare_tests(Module, Filter, Mode) :-
    get_saved_db(Module, Filter),
    get_new_db(Module, Filter),
    compare_tests_(Module, Mode).

get_saved_db(Module, Filter) :-
    cleanup_test_attributes,
    saved_test_input_file_to_test_attributes_db(Module),
    filter_test_attributes_db(Filter),
    cleanup_test_results,
    file_saved_test_output_to_test_results_db(Module),
    cleanup_saved_test_results,
    test_results_db_to_saved_test_results_db.

get_new_db(Module, Filter) :-
    cleanup_test_attributes,
    test_input_file_to_test_attributes_db(Module),
    filter_test_attributes_db(Filter),
    cleanup_test_results,
    file_test_output_to_test_results_db(Module).


difference(brief,_) :- fail.
difference(show,Diff) :-
    message(user, Diff),
    there_are_differences.

compare_tests_(Module, Mode) :-
    retract_fact(test_attributes_db(TestId, Module, F, A, Dict, _, Body, loc(_, LB, LE))), !,
    assertion_body(_,_,_,_,_,Comment,Body),
    ( retract_fact(saved_test_attributes_db(TestId, Module, F, A, Dict, _, Body, loc(_, LB, LE))) ->
        test_description(F,A,Comment,LB,LE,TestMsg),
        compare_test_results(TestId, Mode, TestMsg),
        compare_test_output_error(TestId, Mode, TestMsg)
    ;
        test_description(F,A,Comment,LB,LE,TestMsg),
        difference(Mode, ['\t', 'New test: ', [](TestMsg), '.\n'])
    ),
    compare_tests_(Module, Mode).
compare_tests_(Module, Mode) :-
    retract_fact(saved_test_attributes_db(_TestId, Module, F, A, _, _, Body, loc(_, LB, LE))), !,
    assertion_body(_,_,_,_,_,Comment,Body),
    test_description(F,A,Comment,LB,LE,TestMsg),
    difference(Mode, ['\t', 'Missing test: ', [](TestMsg), '.\n']),
    compare_tests_(Module, Mode).
compare_tests_(_,_).
% TODO: allow locators to vary, make test id depend on test content
% and not locator (e.g., unique id per predicate, plus optional
% user-provided id or number of assertion of that pred)

compare_test_results(TestId, Mode, TestMsg) :-
    retract_fact(test_output_db(TestId, Result)), !,
    ( retract_fact(saved_test_output_db(TestId, SavedResult)) ->
        compare_test_result(SavedResult, Result, Mode, TestMsg)
    ;
        result_message(Result, ResultText),
        difference(Mode, ['\t', 'New result in test ', [](TestMsg), ': ', ResultText, '.\n'])
    ),
    compare_test_results(TestId, Mode, TestMsg).
compare_test_results(TestId, Mode, TestMsg) :-
    retract_fact(saved_test_output_db(TestId, Result)), !,
    result_message(Result, ResultText),
    difference(Mode, ['\t', 'Missing result in test ', [](TestMsg), ': ', ResultText, '.\n']),
    compare_test_results(TestId, Mode, TestMsg).
compare_test_results(_,_,_).

compare_test_result(X,X,_,_) :- !.
compare_test_result(Result1,Result2,Mode,TestMsg) :-
    result_message(Result1, Result1Text),
    result_message(Result2, Result2Text),
    difference(Mode, ['\t', 'Different results in test ', [](TestMsg), ': ', Result1Text, ' and ', Result2Text, '.\n']).

result_message(Result, Result) :-  % TODO: result description
    numbervars(Result, 0, _).

compare_test_output_error(TestId,_,_) :- % reached if there was not output and error in normal and saved output files (e.g., when compilation failed). % TODO: have each test have aborted result and empty stdout and stderr in this case?
    (\+ saved_test_output_error_db(TestId,_,_)),
    (\+ test_output_error_db(TestId,_,_)),
    !.
compare_test_output_error(TestId, brief,_) :- !,
    saved_test_output_error_db(TestId,Output,Error),
    test_output_error_db(TestId,Output,Error).
compare_test_output_error(TestId, show, TestMsg) :-
    saved_test_output_error_db(TestId,SavedOutput,SavedError),
    test_output_error_db(TestId,Output,Error), !,
    % comparing output
    ( Output=SavedOutput -> true ;
        there_are_differences,
        string_to_tmp_file(SavedOutput, 'saved', SavedOutputFile),
        string_to_tmp_file(Output, 'new', OutputFile),
        message(user, ['Output differences in test ', [](TestMsg), ':', '\n']),
        get_tmp_dir(TmpDir),
        process_call(path(diff),
                   ['-U', '2', SavedOutputFile, '--label', 'saved', OutputFile, '--label', 'new'],
                   [status(_)]),
        delete_file(OutputFile),
        delete_file(SavedOutputFile)
    ),
    % comparing error
    ( Error = SavedError -> true ;
        there_are_differences,
        string_to_tmp_file(SavedError, 'saved', SavedErrorFile),
        string_to_tmp_file(Error, 'new', ErrorFile),
        message(user, ['Error differences in test ', [](TestMsg), ':', '\n']),
        get_tmp_dir(TmpDir),
        process_call(path(diff),
                   ['-U', '2', SavedErrorFile, '--label', saved, ErrorFile, '--label', new],
                   [status(_)]),
        delete_file(ErrorFile),
        delete_file(SavedErrorFile)
    ).
compare_test_output_error(_TestId, show,TestMsg) :-
    message(user, ['Missing output/error information in test ', [](TestMsg), '.\n']).

%%%%%%

:- data saved_test_output_db/2.
:- data saved_test_attributes_db/8.
:- data saved_test_output_error_db/3.

cleanup_saved_test_results :-
    retractall_fact(saved_test_output_db(_,_)),
    retractall_fact(saved_test_attributes_db(_,_,_,_,_,_,_,_)),
    retractall_fact(saved_test_output_error_db(_,_,_)).

test_results_db_to_saved_test_results_db :-
    test_attributes_db(A, B, C, D, E, F, G, H),
    assertz_fact(saved_test_attributes_db(A, B, C, D, E, F, G, H)),
    fail.
test_results_db_to_saved_test_results_db :-
    test_output_db(A,B),
    assertz_fact(saved_test_output_db(A,B)),
    fail.
test_results_db_to_saved_test_results_db :-
    test_output_error_db(A,B,C),
    assertz_fact(saved_test_output_error_db(A,B,C)),
    fail.
test_results_db_to_saved_test_results_db.


string_to_tmp_file(Str, Name, TmpFile) :-
    get_tmp_dir(TmpDir),
    path_concat(TmpDir, Name, TmpFile),
    string_to_file(Str, TmpFile).

:- export(test_description/6).
test_description(F,A,Comment,LB,LE,TestMsg) :-
    (Comment = "" -> CommentMsg=[] ;
        CommentMsg = ['"', $$(Comment), '"']),

    TestMsg = ['(', F, '/', A, ' ', [](CommentMsg), ' in lines ', LB, '-', LE, ')'].

% TODO: save output and error toghether, or even save separately
% output, error, and output+error

% TODO: save testout-saved files in a different directory, like .po,
% .asr, .itf, etc. regr_db repo?

