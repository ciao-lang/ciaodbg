:- module(unittest_base, [], [assertions, regtypes, unittestdecls, datafacts]).

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(system), [mktemp_in_tmp/2, delete_file/1]).
:- use_module(library(pathnames), [path_concat/3]).

:- initialization(init_tmp_dir).

:- export(yesno/1).
:- regtype yesno/1.
yesno(yes).
yesno(no).

% show_result_summary((Name=Value)) :-
%       format("~w\t~w\n", [Name, Value]).

% show_results_summary(TestResultsSummary) :-
%       display('Status\tTimes\n'),
%       display('------- --------\n'),
%       maplist(show_result_summary, TestResultsSummary).

% tmp_dir( '/tmp/ciaotest/').

:- export(tmp_dir/1).
:- data tmp_dir/1.

init_tmp_dir :-
    get_test_tmp_dir(TmpDir),
    retractall_fact(tmp_dir(_)),
    assertz_fact(tmp_dir(TmpDir)).

get_test_tmp_dir(TmpDir) :-
    mktemp_in_tmp('ciaotestXXXXXX', TmpDir),
    delete_file(TmpDir).

:- export(runner_global_file_name/1).
runner_global_file_name('test_run_auto.pl').

:- export(wrapper_file_name/3).
wrapper_file_name(TmpDir, Module, WrapperFile) :-
    atom_concat(Module,'_wrp_auto.pl',WrpModule),
    path_concat(TmpDir,WrpModule,WrapperFile).

:- export(make_test_id/5).
make_test_id(Module,_Src,LB,LE,TestId) :-
    atom_number(ALB,LB),
    atom_number(ALE,LE),
    % atom_concat([Module,'#',Src,'#',ALB,'#',ALE],TestId).
    atom_concat([Module,'#',ALB,'#',ALE],TestId).
% TODO: module's might not be unique, include the part of Src that
% does not depend on path to Ciao root

%% The commented out line can be used to save data in text mode and
%% facilitate debugging --EMM
:- compilation_fact(pretty_testout).

:- if(defined(pretty_testout)).

:- use_module(library(read), [read/2]).
:- use_module(library(write), [portray_clause/2]).

:- export(read_data/2).
read_data(SI, Term) :-
    read(SI, Term),
    Term \== end_of_file.

:- export(write_data/2).
write_data(SI, Term) :-
    portray_clause(SI, Term).

:- else.

:- use_module(library(fastrw), [fast_read/2, fast_write/2]).

:- export(read_data/2).
read_data(SI, Term) :-
    fast_read(SI, Term).

:- export(write_data/2).
write_data(SI, Term) :-
    fast_write(SI, Term).

:- endif.

% ---------------------------------------------------------------------------
% Redirection of stdin and stdout for tests

:- use_module(library(stream_utils), [file_to_string/2, write_string/1]).
:- use_module(library(system), [mktemp_in_tmp/2]).

:- export(test_redirect_mode/3).
test_redirect_mode(Std, Options, Mode) :- redirect_opt(Std, Opt, Mode0), member(Opt, Options), !, Mode = Mode0.
test_redirect_mode(_Std, _, save). % default

% TODO: rename options so include std (no need to give different names)
redirect_opt(stdout, dump_output_real_time, dump_rt).
redirect_opt(stderr, dump_error_real_time, dump_rt).
redirect_opt(stdout, dump_output, dump).
redirect_opt(stderr, dump_error, dump).
redirect_opt(stdout, ignore_output, ignore).
redirect_opt(stderr, ignore_error, ignore).
redirect_opt(stderr, error_to_output, to_stdout).

:- export(test_redirect_chn/4).
% Redirection channel (for io_port_reify)
test_redirect_chn(dump_rt, _Std, _, none).
test_redirect_chn(dump, Std, TestRunDir, Chn) :-
    test_redirect_chn(save, Std, TestRunDir, Chn).
test_redirect_chn(save, Std, TestRunDir, file(File)) :-
    test_redirect_file(Std, TestRunDir, File).
test_redirect_chn(ignore, _, _, file(File)) :-
    mktemp_in_tmp('tmpXXXXXX', File). % TODO: add null redirection to io_port_reify?
test_redirect_chn(to_stdout, _, _, stdout).

test_redirect_file(Std, TmpDir, AbsFile) :-
    test_redirect_file_(Std, File),
    path_concat(TmpDir, File, AbsFile).

test_redirect_file_(stdout, 'stdout_redirected').
test_redirect_file_(stderr, 'stderr_redirected').

:- export(test_redirect_contents/4).
% Read contents of redirection
test_redirect_contents(dump_rt, _Std, _, dumped).
test_redirect_contents(dump, Std, TestRunDir, Str) :-
    test_redirect_contents(save, Std, TestRunDir, Str),
    write_string(Str).
test_redirect_contents(save, Std, TestRunDir, Str) :-
    test_redirect_file(Std, TestRunDir, File),
    file_to_string(File, Str).
test_redirect_contents(ignore, _Std, _, ignored).
test_redirect_contents(to_stdout, _Std, _, redirected_to_stdout).

