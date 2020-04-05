:- module(_,
          [
              save_output/1,
              brief_compare/2,
              compare/1
          ],
          [datafacts]).

:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(engine(io_basic), [display/1, nl/0]).
:- use_module(library(write), [print/1]).
:- use_module(library(system), [copy_file/2]).
:- use_module(engine(stream_basic), [flush_output/0]).
:- use_module(library(stream_utils), [get_line/1, string_to_file/2]).
:- use_module(library(process), [process_call/3]).
:- use_module(engine(messages_basic), [messages/1]).
:- use_module(library(system), [mktemp_in_tmp/2, delete_file/1]).

:- use_module(library(unittest/unittest_db),
              [
                  module_base_path_db/3
              ]).
:- use_module(library(unittest/unittest_base),
              [
                  file_test_output_suffix/1,
                  file_test_saved_output_suffix/1
              ]).
:- use_module(library(unittest/unittest_utils), [assert_from_file/2]).

%%%%% save

% copied from octesting
save_output(Modules) :-
    input('Really save output for all modules? (yes/no/) ', Answer),
    ( Answer = "yes" ->
        save_output_(Modules),
        display('Saved'), nl
    ; display('Not saved'), nl
    ).
% TODO: options for asking for comfirmation once, once per module,
% once per test. Ask again if none of those


save_output_([]).
save_output_([M|Ms]) :-
    save_output__(M),
    save_output_(Ms).

save_output__(M) :-
    module_base_path_db(M,Base,_),
    saved_output_file(Base,SavedOutput),
    output_file(Base,Output),
    del_file_nofail(SavedOutput),
    copy_file(Output, SavedOutput).


output_file(Base,Output) :-
    file_test_output_suffix(Suf),
    atom_concat(Base, Suf, Output).

saved_output_file(Base,Output) :-
    file_test_saved_output_suffix(Suf),
    atom_concat(Base, Suf, Output).

input(Question, Answer) :-
    display(Question), nl, flush_output,
    get_line(Answer).

%%%% brief compare

brief_compare([], 0).
brief_compare([M|Ms], ReturnStatus) :-
    brief_compare_(M, MStatus),
    ( MStatus=1 -> ReturnStatus=1 ; ReturnStatus=ReturnStatus0 ),
    brief_compare(Ms, ReturnStatus0).

brief_compare_(M, 0) :-
    clean_output_db,
    get_saved_output_db(M),
    get_new_output_db(M),
    compare_tests(M, brief), !, % there are no differences
    display('Module '), display(M), display(': OK.'), nl.
brief_compare_(M, 1) :- % there are differences
    display('Module '), display(M), display(': There are differences.'), nl.


%%%% compare

compare([]).
compare([M|Ms]) :-
    compare_(M),
    compare(Ms).

compare_(M) :-
    clean_output_db,
    get_saved_output_db(M),
    get_new_output_db(M),
    display('Differences in module '), display(M), display(':'), nl,
    retractall_fact(there_are_differences),
    compare_tests(M, show), % TODO: better messages
    (there_were_differences -> true ; display('None'), nl).

:- data there_were_differences/0.

there_are_differences :-
    (there_were_differences -> true ; assertz_fact(there_were_differences)).

%%%%% compare tests

difference(brief,_) :- fail.
difference(show,Diff) :-
    messages([message(Diff)]),
    there_are_differences.

compare_tests(Module, Mode) :-
    retract_fact(new_test_attributes_db(TestId, Module, F, A, Dict, Comment, _, LB, LE)), !,
    ( retract_fact(saved_test_attributes_db(TestId, Module, F, A, Dict, Comment, _, LB, LE)) ->
        test_description(F,A,Comment,LB,LE,TestMsg),
        compare_test_results(TestId, Mode, TestMsg),
        compare_test_output_error(TestId, Mode, TestMsg)
    ;
        test_description(F,A,Comment,LB,LE,TestMsg),
        difference(Mode, ['\t', 'New test: ', [](TestMsg), '.\n'])
    ),
    compare_tests(Module, Mode).
compare_tests(Module, Mode) :-
    retract_fact(saved_test_attributes_db(TestId, Module, F, A, _, Comment, _, LB, LE)), !,
    test_description(F,A,Comment,LB,LE,_TestMsg),
    difference(Mode, ['\t', 'Missing test: ', TestId, '.\n']),
    compare_tests(Module, Mode).
compare_tests(_,_).
% TODO: allow locators to vary, make test id depend on test content
% and not locator (e.g., unique id per predicate, plus optional
% user-provided id or number of assertion of that pred)

compare_test_results(TestId, Mode, TestMsg) :-
    retract_fact(new_test_output_db(TestId, Result)), !,
    ( retract_fact(saved_test_output_db(TestId, SavedResult)) ->
        compare_test_result(SavedResult, Result, Mode, TestMsg)
    ;
        difference(Mode, ['\t', 'New result in test ', [](TestMsg), ': ', Result, '.\n']) % TODO: result description
    ),
    compare_test_results(TestId, Mode, TestMsg).
compare_test_results(TestId, Mode, TestMsg) :-
    retract_fact(saved_test_output_db(TestId, Result)), !,
    difference(Mode, ['\t', 'Missing result in test ', [](TestMsg), ': ', Result, '.\n']), % TODO: result description
    compare_test_results(TestId, Mode, TestMsg).
compare_test_results(_,_,_).

compare_test_result(X,X,_,_) :- !.
compare_test_result(st(RtChecks1,A,B),st(RtChecks2,A,B),_,_) :-
    compare_rtchecks(RtChecks1,RtChecks2), !.
compare_test_result(X,Y,Mode,TestMsg) :-
    difference(Mode, ['\t', 'Different results in test ', [](TestMsg), ': ', X, ' and ', Y, '.\n']). % TODO: results description

% TODO: make rtchecks locations independent of path to ciao root
compare_rtchecks([],[]).
compare_rtchecks([rtcheck(A,B,C,D,E,Locs1)|RtChecks1], [rtcheck(A,B,C,D,E,Locs2)|RtChecks2]) :-
    compare_locs(Locs1, Locs2),
    compare_rtchecks(RtChecks1, RtChecks2).

compare_locs([],[]).
compare_locs([Loc1|Locs1], [Loc2|Locs2]) :-
    compare_loc(Loc1,Loc2),
    compare_locs(Locs1,Locs2).

compare_loc(_,_). % TODO: implement this

compare_test_output_error(TestId,_,_) :- % reached if there was not output and error in normal and saved output files (e.g., when compilation failed). % TODO: have each test have aborted result and empty stdout and stderr in this case?
    (\+ saved_test_output_error_db(TestId,_,_)),
    (\+ new_test_output_error_db(TestId,_,_)),
    !.
compare_test_output_error(TestId, brief,_) :- !,
    saved_test_output_error_db(TestId,Output,Error),
    new_test_output_error_db(TestId,Output,Error).
compare_test_output_error(TestId, show, TestMsg) :-
    saved_test_output_error_db(TestId,SavedOutput,SavedError),
    new_test_output_error_db(TestId,Output,Error), !,
    % comparing output
    ( Output=SavedOutput -> true ;
        there_are_differences,
        string_to_tmp_file(SavedOutput, SavedOutputFile),
        string_to_tmp_file(Output, OutputFile),
        messages([message(['Output differences in test ', [](TestMsg), ':', '\n'])]),
        process_call(path(diff),
                   [SavedOutputFile, OutputFile],
                   [status(_)]),
        delete_file(OutputFile),
        delete_file(SavedOutputFile)
    ),
    % comparing error
    ( Error = SavedError -> true ;
        there_are_differences,
        string_to_tmp_file(SavedError, SavedErrorFile),
        string_to_tmp_file(Error, ErrorFile),
        messages([message(['Error differences in test ', [](TestMsg), ':', '\n'])]),
        process_call(path(diff),
                   [SavedErrorFile, ErrorFile],
                   [status(_)]),
        delete_file(ErrorFile),
        delete_file(SavedErrorFile)
    ).
compare_test_output_error(_TestId, show,TestMsg) :-
    messages([message(['Missing output/error information in test ', [](TestMsg), '.\n'])]).

%%%%%%

:- data new_test_output_db/2.
:- data new_test_attributes_db/9.
:- data new_test_output_error_db/3.

:- data saved_test_output_db/2.
:- data saved_test_attributes_db/9.
:- data saved_test_output_error_db/3.

clean_output_db :-
    retractall_fact(new_test_output_db(_,_)),
    retractall_fact(new_test_attributes_db(_,_,_,_,_,_,_,_,_)),
    retractall_fact(new_test_output_error_db(_,_,_)),
    retractall_fact(saved_test_output_db(_,_)),
    retractall_fact(saved_test_attributes_db(_,_,_,_,_,_,_,_,_)),
    retractall_fact(saved_test_output_error_db(_,_,_)).
    

get_new_output_db(M) :-
    module_base_path_db(M,Base,_),
    output_file(Base,NewOutput),
    assert_from_file(NewOutput, assert_new_output).

get_saved_output_db(M) :-
    module_base_path_db(M,Base,_),
    saved_output_file(Base,SavedOutput),
    assert_from_file(SavedOutput, assert_saved_output).

assert_new_output(test_output_db(A,B)) :-
    assertz_fact(new_test_output_db(A,B)).
assert_new_output(test_attributes_db(A, B, C, D, E, F, G, H, I)) :-
    assertz_fact(new_test_attributes_db(A, B, C, D, E, F, G, H, I)).
assert_new_output(test_output_error_db(A,B,C)) :-
    assertz_fact(new_test_output_error_db(A,B,C)).

assert_saved_output(test_output_db(A,B)) :-
    assertz_fact(saved_test_output_db(A,B)).
assert_saved_output(test_attributes_db(A, B, C, D, E, F, G, H, I)) :-
    assertz_fact(saved_test_attributes_db(A, B, C, D, E, F, G, H, I)).
assert_saved_output(test_output_error_db(A,B,C)) :-
    assertz_fact(saved_test_output_error_db(A,B,C)).

string_to_tmp_file(Str,TmpFile) :-
    mktemp_in_tmp('tmpXXXXXX',TmpFile),
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
