:- module(unittest_base,
        [
            empty_output/1,
            file_test_input/1,
            file_test_output/1,
            group_list/3,
            make_test_id/5,
            runner_global_file_name/1,
            tmp_dir/1,
            wrapper_file_name/3,
            yesno/1,
            read_data/2,
            write_data/2,
            file_test_output_suffix/1,
            file_test_saved_output_suffix/1
        ],
        [assertions, regtypes, unittestdecls, datafacts]).

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(stream_utils), [string_to_file/2]).
:- use_module(library(system), [mktemp_in_tmp/2, delete_file/1]).
:- use_module(library(pathnames),[path_concat/3]).

:- initialization(init_tmp_dir).

:- doc(author, "Edison Mera").

:- test group_list(A, B, C) : (A = [a, a, b, a, c, d, a, 1, 2, a, 1], B = [])
    => ( C = [count(a, 5), count(b, 1), count(c, 1), count(d, 1),
            count(1, 2), count(2, 1)] ).

group_list([],    M,  M).
group_list([E|L], M0, M) :-
    add_to_list(M0, E, M1),
    !,
    group_list(L, M1, M).

:- test add_to_list(A, B, C) : (A = [count(b, 3), count(a, 1)], B = a)
    => (C = [count(b, 3), count(a, 2)]).

:- regtype yesno/1.

yesno(yes).
yesno(no).

:- export(add_to_list/3).

add_to_list([],               E, [count(E, 1)]).
add_to_list([count(E, N0)|M], E, [count(E, N)|M]) :-
    !,
    N is N0 + 1.
add_to_list([count(E, N)|M0], F, [count(E, N)|M]) :-
    !,
    add_to_list(M0, F, M).

% show_result_summary((Name=Value)) :-
%       format("~w\t~w\n", [Name, Value]).

% show_results_summary(TestResultsSummary) :-
%       display('Status\tTimes\n'),
%       display('------- --------\n'),
%       maplist(show_result_summary, TestResultsSummary).

% tmp_dir( '/tmp/ciaotest/').

:- data tmp_dir/1.

init_tmp_dir :-
    get_test_tmp_dir(TmpDir),
    retractall_fact(tmp_dir(_)),
    assertz_fact(tmp_dir(TmpDir)).

get_test_tmp_dir(TmpDir) :-
    mktemp_in_tmp('ciaotestXXXXXX', TmpDir),
    delete_file(TmpDir).

file_test_output('test_output_auto.pl').
file_test_input('test_input_auto.pl').
runner_global_file_name('test_run_auto.pl').

file_test_output_suffix('.testout').
file_test_saved_output_suffix('.testout-saved').

wrapper_file_name(TmpDir, Module, WrapperFile) :-
    atom_concat(Module,'_wrp_auto.pl',WrpModule),
    path_concat(TmpDir,WrpModule,WrapperFile).

make_test_id(Module,_Src,LB,LE,TestId) :-
    atom_number(ALB,LB),
    atom_number(ALE,LE),
    % atom_concat([Module,'#',Src,'#',ALB,'#',ALE],TestId).
    atom_concat([Module,'#',ALB,'#',ALE],TestId).
% TODO: module's might not be unique, include the part of Src that
% does not depend on path to Ciao root

empty_output(TmpDir) :-
    file_test_output(BOut),
    path_concat(TmpDir,BOut,Out),
    string_to_file("", Out).

%% The commented out lines can be used to save data in text mode and
%% facilitate debugging --EMM
:- use_module(library(fastrw), [fast_read/2, fast_write/2]).
% :- use_module(library(read)).

read_data(SI, Term) :-
    % read(SI, Term),
    % Term \== end_of_file.
    fast_read(SI, Term).

write_data(SI, Term) :-
    % portray_clause(SI, Term).
    fast_write(SI, Term).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% stdout and stderr redirection files %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% used as temporary files for redirecting tests' standard output and
% error

:- export(get_stdout_redirection_file/2).

get_stdout_redirection_file(TmpDir, AbsFile) :-
    stdout_redirection_file(File),
    path_concat(TmpDir,File,AbsFile).

stdout_redirection_file('stdout_redirected').


:- export(get_stderr_redirection_file/2).

get_stderr_redirection_file(TmpDir, AbsFile) :-
    stderr_redirection_file(File),
    path_concat(TmpDir,File,AbsFile).

stderr_redirection_file('stderr_redirected').
