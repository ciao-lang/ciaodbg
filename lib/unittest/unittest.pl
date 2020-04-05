:- module(unittest,
        [
            run_tests_in_module/1,
            run_tests_in_module/2,
            run_tests_in_module/3,
            run_tests_in_module_check_exp_assrts/1,
            run_tests_in_dir_rec/2,
            run_tests_related_modules/1,
            run_tests/3,

            show_untested_exp_preds/1,
            show_test_summaries/1,
            show_test_output/2,
            show_test_related_output/2,

            % regtypes
            test_option/1,
            test_action/1
        ],
        [assertions, regtypes, isomodes, nativeprops, dcg, fsyntax, hiord, datafacts, define_flag]).

:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic), [nl/0]).
:- use_module(library(stream_utils), [write_string/1, file_to_string/2]).
:- use_module(library(streams), [display/1, nl/0]).
:- use_module(engine(messages_basic), [message/2, messages/1]).
:- use_module(library(unittest/unittest_statistics), [statistical_summary/2]).
:- use_module(library(terms),      [atom_concat/2]).
:- use_module(library(sort),       [sort/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(rtchecks/rtchecks_pretty),
    [
        pretty_prop/3,
        rtcheck_to_messages/2
    ]).
:- use_module(library(assertions/assrt_lib),
    [
        cleanup_code_and_related_assertions/0,
        assertion_read/9,
        clause_read/7,
        get_code_and_related_assertions/5,
        assertion_body/7
    ]).
:- use_module(library(system),   [copy_file/2, file_exists/1]).
:- use_module(library(hiordlib), [maplist/2, foldl/4]).
:- use_module(library(compiler/c_itf), [exports/5, defines_module/2]).
:- use_module(library(lists),
    [
        member/2,
        append/3,
        length/2,
        select/3,
        union/3
    ]).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(compiler/exemaker), [make_exec/2]).
:- use_module(library(unittest/unittest_base),
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
        get_stdout_redirection_file/2,
        get_stderr_redirection_file/2,
        file_test_output_suffix/1,
        file_test_saved_output_suffix/1
    ]).
:- use_module(library(unittest/unittest_utils),[assert_from_file/2]).
:- use_module(library(source_tree),
    [
        current_file_find/3,
        remove_dir/1
    ]).
:- use_module(library(pathnames),
    [
        pathname/1,
        path_concat/3,
        path_split/3
    ]).
:- use_module(library(messages),[note_message/2]).


:- use_module(library(unittest/unittest_db),
              [
                  % data
                  module_base_path_db/3,
                  % preds
                  assert_module_under_test/1,
                  clean_db/0
              ]).
:- use_module(library(unittest/unittest_regression),
              [
                  save_output/1,
                  brief_compare/2,
                  compare/1,
                  test_description/6 % move somewhere else
              ]).
:- use_module(library(regrtest/regrtest_aux), [clean_output/3]).

:- export([
    test_output_db/2,
    test_attributes_db/9,
    test_output_error_db/3
]). % temporal, used in unittest_regression and unittest_database


:- doc(title, "Unit testing").

:- doc(author, "Edison Mera").
:- doc(author, "Pedro L@'{o}pez").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Alvaro Sevilla San Mateo").
:- doc(author, "Nataliia Stulova").

:- doc(summary, "Ciao provides an integrated assertion-based
   verification and debugging framework, which includes unit tests.
   In this approach to unit tests, the assertion language is reused to
   provide specifications of test cases for predicates. This library
   contains predicates for running any such tests present in modules
   and for gathering or pretty printing the results.").

:- doc(stability,beta).

:- doc(module, "The Ciao assertion language (see @ref{The Ciao
   assertion language}) allows writing @index{tests} (including
   @index{unit tests}) by means of @index{test assertions}. These
   assertions make it possible to write specific test cases at the
   predicate level. This library contains predicates that can be used
   to run tests in modules and gather or pretty-print the results. It
   also provides some special properties that are convenient when
   writing tests and the corresponding run-time support.

@subsection{Writing test assertions}
@cindex{writing unit tests}

   As described in @ref{The Ciao assertion language} a @index{test
   assertion} is written as follows:

@begin{verbatim}
:- test predicate(A1, A2, ..., An) 
   :  <Precondition>
   => <Postcondition>
   +  <Global properties>
   #  <Comment>.
@end{verbatim}

   Where the fields of the test assertion have the usual meaning in
   Ciao assertions, i.e., they contain conjunctions of properties
   which must hold at certain points in the execution. Here we give a
   somewhat more operational (``test oriented'') reading to these
   fields: 

   @begin{itemize}
   @item @pred{predicate/n} is the predicate to be tested.

   @item @var{Precondition} is a goal (a literal or a conjuntion of
   literals) that is called before the predicate being tested, and can
   be used to generate values of the input parameters. While in some
   other types of assertions these preconditions contain properties to
   be checked, the typical role of the preconditions here is to
   provide concrete input values for which the predicate can be
   actually executed.

   @item @var{Postcondition} is a goal that should succeed after
   @pred{predicate/n} has been called. This is used to test that the
   output of the predicate is the correct one for the input
   provided.

   @item @var{Properties} specifies some global properties that the
   predicate should meet, in the same way as other assertions. For
   example, @code{not_fails} means that the predicate does not fail,
   @code{exception(error(a,b))} means that the predicate should throw
   the exception @code{error(a,b)}, and so on.

   @item @var{Comment} is a string that documents the test.
   @end{itemize}

   The following are some example tests for a complex number evaluator
   (see @ref{Examples (unittest)} for the full code):

@begin{verbatim}
:- module(ceval2, [ceval/2], [assertions, regtypes, nativeprops]).

:- test ceval(A, B) : (A = c(3, 4) + c(1, 2) - c(2, 3))
    => (B = c(2, 3)) + (not_fails, is_det).

:- test ceval(A, B) : (A = c(3, 4) * c(1, 2) / c(1, 2))
    => (B = c(3.0, 4.0)) + (not_fails, is_det).

ceval(A,   A) :- complex(A), !.
ceval(A+B, C) :- ceval(A, CA), ceval(B, CB), add(CA, CB, C).
ceval(A-B, C) :- ceval(A, CA), ceval(B, CB), sub(CA, CB, C).
ceval(A*B, C) :- ceval(A, CA), ceval(B, CB), mul(CA, CB, C).
ceval(A/B, C) :- ceval(A, CA), ceval(B, CB), div(CA, CB, C).

...

:- regtype complex/1.
:- export(complex/1).

complex(c(A, B)) :-
    num(A),
    num(B).
@end{verbatim}

   Test assertions can be combined with other assertions: 

@begin{verbatim}
:- test ceval(A, B) : (A = c(3, 4) + c(1, 2) - c(2, 3))
    => (B = c(2, 3)) + (not_fails, is_det).
:- test ceval(A, B) : (A = c(3, 4) * c(1, 2) / c(1, 2))
    => (B = c(3.0, 4.0)) + (not_fails, is_det).
:- check pred ceval/2 : gnd * term => gnd * complex.
@end{verbatim}

   Test assertions can also take the standard assertion status
   prefixes. In particular, a status of @tt{false} can be used to
   state that a test fails. This can be useful to flag bugs as known.

@begin{verbatim}
:- false test ceval(A, B) : (A = c(3, 4) + c(1, 2) - c(2, 3))
    => (B = c(2, 3)) + (not_fails, is_det).
@end{verbatim}

   Tests with a @tt{false} (or @tt{true}) prefix are not run. 

   There are some specific properties that only apply to testing which
   are provided in module @lib{unittest_props.pl}. For example, the
   limit to the number of solutions to be generated for the tested
   predicate can be set with the property @code{try_sols(N)}, a
   timeout to a test can be set with the property @code{timeout(N)},
   @code{times(N)} specifies that the given test should be executed
   @code{N} times, etc.

@subsection{Running tests in a bundle}

   To run all these tests in a given bundle (as well as the other
   standard tests in the system) run the following (at the top level
   of the source tree or a bundle @cindex{bundle}):

@begin{verbatim}
ciao test
@end{verbatim}

@subsection{Running the tests from the IDE}

   A convenient way to run these tests is by selecting options in the
   @bf{CiaoDbg} menu within the development environment. This menu
   offers the following options: @cindex{running unit tests}

@begin{cartouche}
   @begin{enumerate}

   @item @tt{Run tests in current module}: execute only the tests
     specified in the current module.

   @item @tt{Run tests in current and all related modules}: execute
     the tests specified in the current module and in all the modules
     being used by it.

   @item @tt{Show untested exported predicates}: show the
     @em{exported} predicates that do not have any test assertions.

   @end{enumerate}
@end{cartouche}

@subsection{Running the tests from the top level or programmatically}

   The tests can also be run from the top level, loading this module
   (@lib{unittest.pl}) and calling the appropiate predicates that it
   exports (see the module @ref{Usage and interface} section
   below). This can also be done from a program, provided it imports
   this module.

@subsection{Combination with run-time tests}

   These tests can be combined with the run-time checking of other
   assertions present in the involved modules. This can be done by
   including the @lib{rtchecks} package in the desired modules.  Any
   @tt{check} assertions present in the code will then be checked
   dynamically during the execution of the tests and can detect
   additional errors.

@subsection{Integration tests}

   If you need to write tests for predicates that are spread over
   several modules, but work together, it may be useful to create a
   separate module, and reexport the predicates required to build the
   tests. This allows performing @em{integration testing}, using the
   syntax and functionality of the test assertions.  

").

:- doc(bug, "Currently @bf{only the tests defined for exported
   predicates} are executed.  This is a limitation of the current
   implementation that will be corrected in the future.").

:- push_prolog_flag(write_strings, on).

% -----------------------------

:- use_module(engine(runtime_control), [current_prolog_flag/2]).

define_flag(unittest_default_timeout, integer, 600000). % TODO: move somewhere else?

% -------------------------------
    
%% put to unittest_base together with other file names?
loader_name('ciao_unittest_loader').

% ----------------------------------------------------------------------

:- pred cleanup_unittest(TestRunDir) : pathname(TestRunDir).
cleanup_unittest(TestRunDir) :-
    cleanup_code_and_related_assertions,
    cleanup_test_attributes,
    cleanup_global_runners(TestRunDir).

cleanup_test_attributes :-
    retractall_fact(test_attributes_db(_, _, _, _, _, _, _, _, _)).

:- pred cleanup_global_runners(TestRunDir) : pathname(TestRunDir).
cleanup_global_runners(TestRunDir) :-
    % tests directory must exist
    ( file_exists(TestRunDir)
    -> file_test_input(InputBase),
       path_concat(TestRunDir,InputBase,InputFile),
       % tests directory must always contain input files
       ( file_exists(InputFile)
       -> remove_dir(TestRunDir)
       ; note_message("~w does not look like a tests source directory, aborting.",[TestRunDir]),
         throw(error(existence_error(source_sink,InputBase), cleanup_global_runners/1-1))
       )
    ; true % nothing to clean
    ).

% ----------------------------------------------------------------------

:- pred show_untested_exp_preds(Alias) : sourcename(Alias)
# "Show any exported predicates that do not have test assertions.
   This is an aid towards ensuring that all exported predicates have
   tests.".

show_untested_exp_preds(Alias) :-
    tmp_dir(TestRunDir),
    cleanup_unittest(TestRunDir),
    get_assertion_info(current, Alias, _Modules),
    findall(Message, current_untested_pred(Alias, Message), Messages),
    % TODO: rtchecks_pretty:compact_list/2 was called here, needed?
    messages(Messages).

:- regtype unittest_type/1.

unittest_type(test).
unittest_type(texec).

:- pred current_untested_pred(Alias, Message)
    : ( sourcename(Alias), var(Message) )
    => struct(Message).
current_untested_pred(Alias, Message) :-
    absolute_file_name(Alias, '_opt', '.pl', '.', FileName, FileBase,
        AbsDir),
    path_concat(AbsDir,Module,FileBase),
    exports(FileBase, F, A, _DefType, _Meta),
    functor(Pred, F, A),
    \+ (
        assertion_read(Pred, Module, check, Type, _Body, _Dict,
            _Source, _LB, _LE),
        unittest_type(Type)
    ),
    (clause_read(FileBase, Pred, _, _, FileName, LB, LE) -> true),
    Message = message_lns(FileName, LB, LE, warning,
        [Module, ':', F, '/', A, ' does not have any unit test']).


:- pred run_tests_in_dir_rec(BaseDir, Opts) : pathname * list(test_option)
# "Executes all the tests in the modules of the given directory and
   its subdirectories. You can indicate that the modules in a
   sub-directory should not be tested by placing an empty NOTEST file
   in that sub-directory.  Also, if a NOTESTFILES file is present,
   containing patterns for modules, those modules will not be tested.".

run_tests_in_dir_rec(BaseDir, Opts) :-
    run_tests(BaseDir, [dir_rec | Opts], [check, show_output, show_stats]).

module_base(Module, Base) :-
    defines_module(Base, Module),
    !.
module_base(Module, Base) :-
    (
        unittest_type(Type),
        assertion_read(_, Module, check, Type, _, _, Base, _, _),
        atom_concat([_, '/', Module, '.pl'], Base) -> true
    ;
        fail
    ).

% ----------------------------------------------------------------------
:- doc(test_option/1,"A global option that controls the
    testing system. The current set of options is:

    @begin{itemize}

    @item @tt{dump_output}: Show the standard output of the test execution.

    @item @tt{dump_error}: Show the standard error of the test execution.

    @item @tt{rtc_entry}: Force run-time checking of at least exported
          assertions even if the runtime_checks flag has not been
          activated. (This is a workaround since currently we cannot
          enable runtime checks in system libraries smoothly).
   
    @item @tt{treat_related} : Run tests in current and all related modules;

    @item @tt{dir_rec} : Run tests in a specified directory recursively.

    @end{itemize}").

:- regtype test_option(Opt) # "@var{Opt} is a testing option.".

test_option := dump_output   | dump_error | rtc_entry.
test_option := treat_related | dir_rec.

:- doc(test_action/1, "A global option that specifies a testing
    routine. The current set of actions is:

    @begin{itemize}

    @item @tt{check} : run tests and temporarily save
          results in the auto-rewritable @tt{module.testout} file;

    @item @tt{show_output} : print the testing trace to the
          standard output;

    @item @tt{show_stats} : print the test results statistics to
          the standard output;

    @item @tt{save} : save test results file in
              @tt{module.testout-saved} file;

    @item @tt{briefcompare} : check whether current and saved test
              output files differ;

    @item @tt{compare} : see the differences in the current and saved
              test output files in the diff format;

    @end{itemize}").

:- regtype test_action(Action) # "@var{Action} is a testing action".

test_action := check | show_output | show_stats .
% test_action := show_output_short | show_output_full % TODO: verbosity control
test_action := save | briefcompare | compare .

get_test_opt(Opt, YesNo, Opts) :- member(Opt, Opts), !, YesNo = yes.
get_test_opt(_  , no   , _).

% ----------------------------------------------------------------------

:- pred show_test_output(Alias, Format) : (sourcename(Alias), atm(Format))
    # "Given a file @var{Alias}, tries to lookup the respective
      unittest output file and print it to the standard output in
      the specified @var{Format} ('output' for test full trace, 'stats'
      for a statistical summary only, 'full' for both), otherwise emits
      a warning message that no test output file is avaiable.".
% TODO: it is false that it emits a warning message if there is not
% output

show_test_output(Alias, Format) :-
    show_test_output_(current, Alias, Format).
% TODO: rewrite as run_tests(Alias, [], [show_output/show_stats])

show_test_related_output(Alias, Format) :-
    show_test_output_(related, Alias, Format).
% TODO: rewrite as run_tests(Alias, [treat_related], [show_output/show_stats])

show_test_output_(TestMode, Alias, Format) :-
    cleanup_code_and_related_assertions,
    get_assertion_info(TestMode, Alias, Modules),
    get_all_test_outputs(Modules, new, TestResults),
    show_test_output_format(Format, TestResults).

show_test_output_format(output, TestResults) :-
    show_test_summaries(TestResults).
show_test_output_format(stats, TestResults) :-
    statistical_summary(['{Total:\n'], TestResults).
show_test_output_format(full, TestResults) :-
    show_test_summaries(TestResults),
    statistical_summary(['{Total:\n'], TestResults).

:- pred show_test_summaries(TestSummaries)
   # "Pretty print the test results contained in @var{TestSummaries}.".

show_test_summaries(IdxTestSummaries0) :-
    % TODO: multiple test results bug
    flatten(IdxTestSummaries0, IdxTestSummaries),
    foldl(process_runtime_check, IdxTestSummaries, Messages, []),
    % TODO: rtchecks_pretty:compact_list/2 was called here, needed?
    messages(Messages).

% ----------------------------------------------------------------------

% TODO: ensure Opts and Actions are valid
run_tests(Target, Opts, Actions) :-
    clean_db,
    decide_modules_to_test(Target, Opts, Modules),
    % run the tests
    ( member(check, Actions) ->
      run_tests_in_all_modules(Modules, Opts)
    ; true
    ),
    % show tested predicates' output
    ( member(dump_output, Opts) -> % TODO: make it an action, not an option
        dump_output(Modules, new)
    ; true
    ),
    % show tested predicates' error
    ( member(dump_error, Opts) -> % TODO: make it an action, not an option
        dump_error(Modules, new)
    ; true
    ),
    % TODO: action to show tested predicates output and error together
    %
    % show_test results
    ( member(show_output, Actions) ->
        get_all_test_outputs(Modules, new, TestResults),
        show_test_output_format(output, TestResults)
    ; true
    ),
    % show test statistics
    ( member(show_stats, Actions) ->
        get_all_test_outputs(Modules, new, TestResults),
        show_test_output_format(stats, TestResults)
    ; true
    ),
    ( member(save, Actions) ->
        save_output(Modules)
    ; true
    ),
    ( member(briefcompare(ReturnStatus), Actions) ->
        brief_compare(Modules, ReturnStatus)
    ; true
    ),
    ( member(briefcompare, Actions) ->
        brief_compare(Modules, _)
    ; true
    ),
    ( member(compare, Actions) ->
        compare(Modules)
    ; true
    ),
    ( member(dump_saved_output, Actions) ->
        dump_output(Modules, saved)
    ; true
    ),
    ( member(dump_saved_error, Actions) ->
        dump_error(Modules, saved)
    ; true
    ),
    ( member(show_saved_output, Actions) ->
        get_all_test_outputs(Modules, saved, TestResults),
        show_test_output_format(output, TestResults)
    ; true
    ),
    ( member(show_saved_stats, Actions) ->
        get_all_test_outputs(Modules, saved, TestResults),
        show_test_output_format(output, TestResults)
    ; true
    ).

decide_modules_to_test(Target, Opts, Modules) :-
    assert_modules_to_test(Target, Opts),
    findall(Path-Module,
            (module_base_path_db(Module,_,Path)),
            PathsModules0),
    sort(PathsModules0, PathsModules), % because order is not stable across different intallations (mine and Gitlab's)
    unzip(PathsModules,_,Modules).

unzip([],[],[]).
unzip([A-B|L],[A|As], [B|Bs]) :- unzip(L,As,Bs).
% asserts in module_base_path/3 facts the modules that are required
% to be tested
assert_modules_to_test(Target, Opts) :-
    ( member(dir_rec, Opts) ->
        (  % (failure-driven loop)
            current_file_find(testable_module, Target, Path),
              assert_module_under_test(Path),
            fail
        ;
            true
        )
    ;
        assert_module_under_test(Target)
    ),
    (
        member(treat_related, Opts), % not compatible yet with dir_rec if there are modules with the same name
        current_fact(module_base_path_db(_,_,Path)),
          get_related_modules(Path,RelatedModules),
          member(Mod,RelatedModules),
            \+ module_base_path_db(Mod,_,_), % TODO: we also need the path since module might not be unique
            assert_module_under_test(Mod),
        fail
    ;
        true
    ).

get_related_modules(FileName,RelatedModules) :- % TODO: do it right, and return also paths since modules might not be unique
    cleanup_code_and_related_assertions,
    get_assertion_info(related,FileName,RelatedModules).

run_tests_in_all_modules([], _).
run_tests_in_all_modules([Module|Modules], Opts) :-
    run_tests_in_one_module(Module, Opts),
    run_tests_in_all_modules(Modules, Opts).
% TODO: option for running tests for more than one module at once

run_tests_in_one_module(Module, Opts) :-
    tmp_dir(TestRunDir),
    cleanup_unittest(TestRunDir), % TODO: check if module is already read before cleaning and reading again
    module_base_path_db(Module,_,Path),
    get_assertion_info(current,Path,[Module]),
    run_test_assertions(TestRunDir, [Module], Opts).


get_output_file(new, Base, File) :-
    file_test_output_suffix(Suf),
    atom_concat(Base, Suf, File).
get_output_file(saved, Base, File) :-
    file_test_saved_output_suffix(Suf),
    atom_concat(Base, Suf, File).

dump_output([], _).
dump_output([Module|Modules], WhichOutput) :-
    dump_output_(Module, WhichOutput),
    dump_output(Modules, WhichOutput).

dump_output_(Module, WhichOutput) :-
    module_base_path_db(Module, Base, _),
    get_output_file(WhichOutput, Base, FileModOut),
    retractall_fact(test_output_error_db(_,_,_)),
    assert_from_file(FileModOut, assert_test_output_error),
    retractall_fact(test_attributes_db(_,_,_,_,_,_,_,_,_)),
    assert_from_file(FileModOut, assert_test_attributes),
    ( % (failure-driven) loop
        retract_fact(test_attributes_db(TestId,_,F,A,_,Comment,_,LB,LE)),
        retract_fact(test_output_error_db(TestId,Output,_)),
          ( Output=[] -> true ;
              test_description(F,A,Comment,LB,LE,TestMsg),
              messages([message(note, ['Output in test ', [](TestMsg), ':', '\n'])]),
              write_string(Output)
          ),
        fail
    ;
        true
    ).

dump_error([],_).
dump_error([Module|Modules],WhichOutput) :-
    dump_error_(Module, WhichOutput),
    dump_error(Modules, WhichOutput).

dump_error_(Module, WhichOutput) :-
    module_base_path_db(Module, Base, _),
    get_output_file(WhichOutput, Base, FileModOut),
    retractall_fact(test_output_error_db(_,_,_)),
    assert_from_file(FileModOut, assert_test_output_error),
    retractall_fact(test_attributes_db(_,_,_,_,_,_,_,_,_)),
    assert_from_file(FileModOut, assert_test_attributes),
    ( % (failure-driven) loop
        retract_fact(test_attributes_db(TestId,_,F,A,_,Comment,_,LB,LE)),
        retract_fact(test_output_error_db(TestId,_,Error)),
          ( Error=[] -> true ;
              test_description(F,A,Comment,LB,LE,TestMsg),
              messages([message(note, ['Error in test ', [](TestMsg), ':', '\n'])]),
              write_string(Error)
          ),
        fail
    ;
        true
    ).
% TODO: exceptions and rtchecks intercepted by test runner are not
% further printed to error, as it would happen in a normal execution
% of the predicate

% TODO: allow showing output and error together

% ----------------------------------------------------------------------

:- pred run_tests_in_module(Alias, Opts, TestSummaries)
    : (sourcename(Alias), list(test_option,Opts))
    => list(TestSummaries)
# "Run the tests in module @var{Alias} (with options @var{Opts}).  The
   results of the tests are returned as data in
   @var{TestSummaries}. @var{TestSummaries} can be pretty printed
   using @pred{show_test_summaries/1} and
   @pred{statistical_summary/2}.".

run_tests_in_module(Alias, Opts, TestSummaries) :-
    run_tests(Alias, Opts, [check]),
    get_assertion_info(current, Alias, Modules),
    get_all_test_outputs(Modules, new, TestSummaries).

:- pred run_tests_in_module(Alias, Opts)
    : (sourcename(Alias), list(test_option,Opts))
# "Run the tests in module @var{Alias}. The results of the tests are
   printed out.".
run_tests_in_module(Alias, Opts) :-
     run_tests(Alias, Opts, [check, show_output, show_stats]).

:- pred run_tests_in_module(Alias) : sourcename(Alias)
# "Run the tests in module @var{Alias} (using default options).  The
   results of the tests are printed out.".

run_tests_in_module(Alias) :-
    run_tests(Alias, [], [check, show_output, show_stats]).

run_tests_in_module_check_exp_assrts(Alias) :-
    run_tests(Alias, [rtc_entry], [check, show_output, show_stats]).

:- pred run_tests_related_modules(Alias) : sourcename(Alias).

run_tests_related_modules(Alias) :-
    run_tests(Alias, [treat_related], [check, show_output, show_stats]).

% ----------------------------------------------------------------------

:- export(get_assertion_info/3).
:- doc(hide,get_assertion_info/3).
:- pred get_assertion_info(TestMode, Alias, Modules)
    : ( atm(TestMode), sourcename(Alias), var(Modules) )
    =>  list(atm, Modules)
 # "Read related assertions of source at @var{Alias} into database and
    get the test module name @var{Modules} if the testing is done only
    for the current module (@var{TestMode} = @tt{current}) or get a
    list of all realted test module names (@var{TestMode} =
    @tt{related})".
get_assertion_info(current, Alias, [Module]) :-
    absolute_file_name(Alias, '_opt', '.pl', '.', FileName, Base, AbsDir),
    path_split(Base, AbsDir, Module),
    get_code_and_related_assertions(FileName, Module, Base, '.pl', AbsDir).
get_assertion_info(related, Alias, Modules) :-
    absolute_file_name(Alias, FileName),
    get_code_and_related_assertions(FileName, _, _, _, _),
    set_of_modules(Modules).

set_of_modules := ~sort(~findall(Module, current_assr_module(Module))).

current_assr_module(Module) :-
    assertion_read(_A, Module, check, Type, _E, _F, _G, _H, _I),
    unittest_type(Type).


:- use_module(ciaobld(config_common), [libcmd_path/4]).
unittest_exec := ~libcmd_path(ciaodbg, plexe, 'unittest_runner').

:- use_module(ciaobld(cpx_process), [cpx_process_call/3]).
invoke_unittest(WrapperMods, InputPath, Args0, Opts) :-
    append(WrapperMods, ['--end_wrapper_modules--', InputPath|Args0], Args),
    cpx_process_call(~unittest_exec, Args, Opts).


:- pred run_test_assertions(+pathname, +list(atm), +list) +
    (not_fails, no_choicepoints).

run_test_assertions(TestRunDir, Modules, Opts) :-
    mkpath(TestRunDir),
    create_test_input(TestRunDir, Modules),
    file_test_input(BInFile),
    path_concat(TestRunDir, BInFile, InFile),
    retractall_fact(test_input_db(_, _)),
    assert_from_file(InFile, assert_test_input),
    %
    empty_output(TestRunDir),
    ( test_attributes_db(_, _, _, _, _, _, _, _, _) ->
      get_test_opt(rtc_entry, RtcEntry, Opts),
      create_wrapper_mods(Modules, TestRunDir, RtcEntry, WrapperMods),
      do_tests(TestRunDir, Modules, WrapperMods, Opts)
    ; true
    ),
    % even if module has no tests, we write an empty test output file
    % in order to mark that testing had been performed on a module,
    % which allows detecting newly added tests on regression testing
    write_all_test_outputs(Modules).


write_all_test_outputs([]).
write_all_test_outputs([Module|Mods]) :-
    module_base(Module, Base),
    write_module_output(Module, Base),
    write_all_test_outputs(Mods).

write_module_output(Module, Base) :-
    file_test_output_suffix(Suf),
    atom_concat(Base, Suf, FileModOut),
    open(FileModOut, write, StreamOut),
    write_testdata_to_outfile(StreamOut, Module),
    close(StreamOut).

write_testdata_to_outfile(StreamOut, Module) :-
    ( % (failure-driven loop)
        test_input_db(TestId, Module), % backtracking here
          retract_fact(test_attributes_db(TestId, Module, A,B,C,D,E,F,G)),
          write_data(StreamOut, test_attributes_db(TestId, Module, A,B,C,D,E,F,G)),
          retract_fact(test_output_error_db(TestId, Output, Error)),
          write_data(StreamOut, test_output_error_db(TestId, Output, Error)),
          retract_fact(test_output_db(TestId, TestResult)), % backtracking here
            write_data(StreamOut, test_output_db(TestId, TestResult)),
        fail
    ;
        true
    ).

get_all_test_outputs([], _, []).
get_all_test_outputs([Module|Modules], WhichOutput, [TestResult|TestResults]) :-
    get_module_output(Module, WhichOutput, TestResult),
    get_all_test_outputs(Modules, WhichOutput, TestResults).

get_module_output(Module, WhichOutput, TestResult) :-
    (module_base(Module, Base) -> true ; module_base_path_db(Module, Base, _)),
    % TODO: redefine show_test_output/2 as run_tests(Alias,[show_output/show_stats],[]) and use only mod_base_file_db
    get_output_file(WhichOutput, Base, FileModOut),
    retractall_fact(test_output_db(_, _)),
    retractall_fact(test_attributes_db(_, _, _, _, _, _, _, _, _)),
    assert_from_file(FileModOut, assert_test_output),
    assert_from_file(FileModOut, assert_test_attributes),
    findall(IdxTestSummary,
            get_one_test_assertion_output(Module, IdxTestSummary),
            TestResult).

:- pred get_one_test_assertion_output(Module, IdxTestSummary)
    :  atm(Module)
    => struct(IdxTestSummary).

get_one_test_assertion_output(Module, TestAttributes-TestSummary) :-
    test_attributes_db(TestId, Module, F, A, Dict, Comment, Source, LB, LE),
    findall(TestResult, test_output_db(TestId, TestResult), TestResults),
    group_list(TestResults, [], TestSummary),
    TestAttributes = test_attributes(Module, F, A, Dict, Comment,
                                     Source, LB, LE).

count_text(1, '') :- !.
count_text(N, [' ', N, ' times']).

signals_text([],      '') :- !.
signals_text(Signals, [' Signals thrown: ', ~~(Signals)]).

comment_text("",      '') :- !.
comment_text(Comment, [' <<', $$(Comment), '>>']).

:- pred process_runtime_check(TATS, M0, M) : nonvar(TATS) => nonvar(M0)
    + not_fails.

process_runtime_check(TestAttributes-TestSummary) -->
    {TestAttributes = test_attributes(Module, F, A, Dict, Comment,
            Source, LB, LE)},
    foldl(process_runtime_check_ta(Module, F, A, Dict, Comment, Source, LB, LE),
          TestSummary).

process_runtime_check_ta(Module, F, A, Dict, Comment, Source, LB, LE, count(ErrorStatus, Count)) -->
    {ErrorStatus = st(RTCErrors, Signals0, Result0)},
    {pretty_prop(t(Result0, Signals0), Dict, t(Result, Signals))},
    {count_text(Count, CountMsg)},
    {signals_text(Signals, SignalsMsg)},
    {comment_text(Comment, CommentMsg)},
    (
        {is_failed_test(ErrorStatus)} ->
        [message_lns(Source, LB, LE, error, [Module, ':', F, '/', A,
                    ' (Result: ', ''({Result}), [](CountMsg),
                    ') Failed test', [](CommentMsg), '.', [](SignalsMsg)])
        ],
        foldl(rtcheck_to_messages_, RTCErrors)
    ;
        [message_lns(Source, LB, LE, note, [Module, ':', F,
                    '/', A, ' (Result: ', ''({Result}), [](CountMsg),
                    ') Passed test', [](CommentMsg), '.', [](SignalsMsg)])]
    ),
    !.

rtcheck_to_messages_(E, Msgs, Msgs0) :-
    rtcheck_to_messages(E, Ys),
    append(Ys, Msgs0, Msgs).

is_failed_test(st([_|_], _, _)) :- !.
is_failed_test(st(_,     _, Result)) :- is_failed_test_result(Result).

% TODO: treat PANIC in special way, other than test failure.
% TODO: similar behavior should be in rtchecks
is_failed_test_result(aborted(_, _)). % TODO: global property aborts/1 (like Major Exception), treat as test fail
is_failed_test_result(fail(precondition)).  % Show warning
is_failed_test_result(exception(precondition, _)). % PANIC
is_failed_test_result(exception(postcondition, _)). % PANIC
is_failed_test_result(exception(predicate, timeout)). % PANIC
% TODO: is_failed_test_result(exception(predicate, _)).  assertions should assume no_exception/1 by default.
% TODO: timeouts and exceptions in general need to be treated better

:- data test_input_db/2.
:- data test_output_db/2.
:- data test_attributes_db/9.
:- data test_output_error_db/3.

assert_test_input(test_input_db(A, B)) :-
    assertz_fact(test_input_db(A, B)).

assert_test_output(test_output_db(A, B)) :-
    assertz_fact(test_output_db(A, B)).

assert_test_attributes(test_attributes_db(A, B, C, D, E, F, G, H, I)) :-
    assertz_fact(test_attributes_db(A, B, C, D, E, F, G, H, I)).

assert_test_output_error(test_output_error_db(TestId, Output, Error)) :-
    assertz_fact(test_output_error_db(TestId, Output, Error)).

:- pred do_tests(TestRunDir, Modules, WrapperMods, RunnerArgs)
    :  pathname * list(atom) * list * list(test_option)
# "Calls the runner as an external process. If some test aborts, calls
   recursively with the rest of the tests".
do_tests(TestRunDir, Modules, WrapperMods, RunnerArgs) :-
    do_tests_(TestRunDir, Modules, WrapperMods, RunnerArgs, no).

do_tests_(TestRunDir, Modules, WrapperMods, RunnerArgs, Resume) :-
    current_prolog_flag(unittest_default_timeout,TimeoutN),
    atom_number(TimeoutAtm,TimeoutN),
    RunnerArgs2 = [timeout,TimeoutAtm |RunnerArgs],
    ( Resume = yes(ContIdx) ->
        RunnerArgs3 = [resume_after,ContIdx |RunnerArgs2]
    ; RunnerArgs3 = RunnerArgs2
    ),
    % this process call appends new outputs to OutFile
    invoke_unittest(WrapperMods, TestRunDir ,RunnerArgs3,
                 [stdin(null),
                  stdout(string(StrOut)),
                  stderr(string(StrErr)),
                  status(Status)]),
    %
    (Status==101 -> % returned by unittest_runner.pl when a wrapper module does not compile
        message(error, ['Compilation failed. Please make sure all relevant predicates',
                        ' and properties for testing are exported. Compilation log:']),
        write_string(StrOut),
        write_string(StrErr)
        % Note: Syntactic compilation errors of the modules under test
        % are detected earlier, in the call to
        % get_code_and_related_assertions/5 in the predicate
        % get_assertion_info/3.
        %
        % TODO: this will only show errors and warnings if compilation
        % fails (i.e, if there are errors, but not if there are only
        % warnings). These includes warnings specific to unittest
        % (e.g., texec test with comp property)
        %
        % TODO: write aborted results for each tests or abort
        % completely. Right now it prints the messages but it shows
        % the tests as passed in the stadistics
        %
        % TODO: save compilation status in test.out
        %
        % TODO: unique return status for run_tests_in_module/3
    ;
        file_test_output(BOutFile),
        path_concat(TestRunDir, BOutFile, OutFile),
        retractall_fact(test_output_db(_, _)),
        assert_from_file(OutFile, assert_test_output),
        retractall_fact(test_output_error_db(_, _, _)),
        assert_from_file(OutFile, assert_test_output_error),
        ( aborted_test(TestId)
          ->  % no output both in output file and output db (test aborted)
              get_stdout_redirection_file(TestRunDir, StdoutFile),
              file_to_string(StdoutFile, StdoutString),
              get_stderr_redirection_file(TestRunDir, StderrFile),
              file_to_string(StderrFile, StderrString),
              TestResult = st([], [], aborted(StrOut, StrErr)),
              % mark the test as aborted
              open(OutFile, append, IO),
              write_data(IO, test_output_db(TestId, TestResult)),
              write_data(IO, test_output_error_db(TestId, StdoutString, StderrString)),
              close(IO),
              % continue testing
              do_tests_(TestRunDir, Modules, WrapperMods, RunnerArgs, yes(TestId))
          ; true % (all tests had output)
          )
    ).

aborted_test(TestId) :- % TODO: have a less of a hack mechanism to detect this
    test_input_db(TestId0,_Module),
    \+(test_output_error_db(TestId0,_Output,_Error)), !,
    TestId = TestId0.

% :- pred atom_concat_(+atm,+atm,-atm) + (not_fails, no_choicepoints).

% atom_concat_(A,B,C) :- atom_concat(A,B,C).

:- pred create_test_input(+pathname, +list(atm)) + (not_fails, no_choicepoints).

create_test_input(TestRunDir, Modules) :-
    file_test_input(BFileTestInput),
    path_concat(TestRunDir, BFileTestInput, FileTestInput),
    cleanup_test_attributes,
    open(FileTestInput, write, SI),
    (
        member(Module, Modules),
        get_test(Module, TestId, _Type, Pred, Body, Dict, Src, LB, LE),
        assertion_body(_Pred, _, _, _, _, Comment, Body),
        functor(Pred, F, A),
        assertz_fact(test_attributes_db(TestId, Module, F, A, Dict, Comment,
                Src, LB, LE)),
        write_data(SI, test_input_db(TestId, Module)),
        fail
    ;
        close(SI)
    ),
    atom_concat(FileTestInput, '.bak', FileTestInput0),
    copy_file(FileTestInput, FileTestInput0).

:- pred get_test(Module, TestId, Type, Pred, Body, Dict, Src, LB, LE )
    :  atm(Module)
    => atm * atm * unittest_type * term * term * term * atm * int * int
    + non_det
 # "Given a module name @var{Module}, return one test assertion with
  the identifier @var{TestId} with respective assertion parameters
  (assertion body @var{Body}, type @var{Type}, variable dictionary
  @var{Dict}, module source path @var{Src}, start line @var{LB} and
  end line @var{LE}) for a predicate @var{Pred}".

get_test(Module, TestId, Type, Pred, Body, Dict, Src, LB, LE) :-
    current_fact(assertion_read(Pred, Module, check, Type,
                                Body, Dict,   Src,   LB, LE)),
    unittest_type(Type),
    make_test_id(Module, Src, LB, LE, TestId).

create_wrapper_mods([], _, _, []) :- !.
create_wrapper_mods([Module|Ms], TestRunDir, RtcEntry, [WrapperFile|WFs]) :-
    create_wrapper_mod(Module, TestRunDir, RtcEntry, WrapperFile),
    create_wrapper_mods(Ms, TestRunDir, RtcEntry, WFs).

create_wrapper_mod(Module, TestRunDir, RtcEntry, WrapperFile) :-
    module_base(Module, Base),
    ( wrapper_file_name(TestRunDir, Module, WrapperFile),
      create_module_wrapper(TestRunDir, Module, RtcEntry, Base, WrapperFile)
    -> true
    ; message(error, ['Failure in create_wrapper_mod/4'])
    ).

current_test_module(Src, (:- use_module(TestModule))) :-
    clause_read(Src, 1, load_test_module(TestModule), _, _, _, _).
current_test_module(Src, (:- use_module(TestModule, Predicates))) :-
    clause_read(Src, 1, load_test_module(TestModule, Predicates), _, _, _,
        _).
current_test_module(Src, (:- use_package(TestModule))) :-
    clause_read(Src, 1, load_test_package(TestModule), _, _, _, _).

collect_test_modules(Src) :=
    ~sort(~findall(TestModule, current_test_module(Src, TestModule))).

% We create module wrappers that contains the test entries from the original source.
% In that way the original modules are not polluted with test code.
create_module_wrapper(TestRunDir, Module, RtcEntry, Src, WrapperFile) :-
    Header = [
            (:- module(_, _, [assertions, nativeprops, rtchecks])),
            (:- use_module(library(unittest/unittest_runner_aux))),
            (:- use_module(library(rtchecks/rtchecks_rt))),
            (:- use_module(library(rtchecks/rtchecks_basic))),
            (:- use_module(library(unittest/unittest_props))),
            (:- use_module(Src)),
            (:- multifile internal_runtest_module/2)
    ],
    collect_test_modules(Src, TestModules),
    % here link the TestEntry clause with the ARef test identifier
    TestEntry = internal_runtest_module(Module, ARef),
    findall(Clause,
            gen_test_entry(TestRunDir, Module, RtcEntry, Src, TestEntry, ARef, Clause),
            Clauses),
    %
    Clauses2 = ~flatten([
        raw(Header),
        raw(TestModules),
        raw([(:- push_prolog_flag(single_var_warnings, off))]),
        Clauses,
        raw([(:- pop_prolog_flag(single_var_warnings))])]),
    %
    print_clauses_to_file(WrapperFile, Clauses2).

% TODO: remove this dependency --NS
:- use_module(library(rtchecks/rtchecks_tr), [collect_assertions/3]).

gen_test_entry(TestRunDir, Module, RtcEntry, Src, TestEntry, ARef, Clause) :-
    % test entries, or default failing clause if there are none
    if(do_gen_each_test_entry(TestRunDir, Module, RtcEntry, Src, TestEntry, ARef, Clause),
       true,
       do_gen_default_test_entry(TestEntry, Clause)).

do_gen_default_test_entry(TestEntry, Clause) :- % Why is this clause needed?
    Clause = clause(TestEntry, fail, []).

% TODO: tests abort when the predicate is not defined in the module,
%       fix?  Depends on if we allow tests for imports -- otherwise
%       this code is still useful for writing test assertions of
%       impldef preds such as foreign.
do_gen_each_test_entry(TestRunDir, Module, RtcEntry, Src, TestEntry, TestId, Clause) :-
    % get current test assertion
    get_test(Module, TestId, AType, Pred, ABody, ADict, ASource, ALB, ALE),
    TestInfo = testinfo(TestId, AType, Pred, ABody, ADict, ASource, ALB, ALE),
    % Collect assertions for runtime checking during unit tests:
    %  - none if RtcEntry = no
    %  - none if the module uses rtchecks (already instruments its predicates)
    %  - otherwise, the assertions specified in the original module
    %
    ( (clause_read(Src, 1, rtchecked, _, _, _, _) ; RtcEntry == no) ->
          Assertions = []
    ; collect_assertions(Pred, Module, Assertions)
    ),
    % Get predicate locator for the Pred from the test assertion TestId
    functor(Pred, F, A),
    functor(Head, F, A),
    ( clause_read(Src, Head, _, _, PSource, PLB, PLE) ->
        PLoc = loc(PSource, PLB, PLE)
    ; PLoc = none
    ),
    % this term is later expanded in the wrapper file by the goal
    % translation of the rtchecks package
    TestBody = '$test_entry_body'(TestInfo, Assertions, PLoc, TestRunDir),
    Clause = clause(TestEntry, TestBody, ADict).


:- pop_prolog_flag(write_strings).

% ---------------------------------------------------------------------------
% Generate modules from terms

:- use_module(library(write), [printq/1]).
:- doc(hide,portray/1).
:- multifile portray/1.
portray('$stream'(Int1, Int2)) :-
    integer(Int1),
    integer(Int2),
    !,
    printq('$stream'(int, int)).
portray(attach_attribute(X, float(V))) :-
    !,
    printq(X), printq('.=.'), printq(V).
portray(rat(A, B)) :-
    integer(A),
    integer(B),
    !,
    printq(A/B).

:- use_module(library(varnames/apply_dict), [apply_dict/3]).
:- use_module(library(write), [write/1, writeq/1]).

% (exported)
print_clauses_to_file(Path, Clauses) :-
    open(Path, write, IO),
    print_clauses(Clauses, IO),
    close(IO).

print_clauses([], _IO).
print_clauses([C|Cs], IO) :- print_clause(C, IO), print_clauses(Cs, IO).

print_clause(raw(Clauses), IO) :-
    unittest_print_clauses(Clauses, IO, []).
print_clause(clause(Head, Body, Dict), IO) :-
    unittest_print_clause(IO, Dict, (Head :- Body)).

% unittest_print_clause(S, _Dict, Term) :-
%       current_output(CO),
%       set_output(S),
%       writeq(Term),
%       write('.'),
%       nl,
%       set_output(CO).

unittest_print_clause(S, Dict, Term) :-
    apply_dict(Term, Dict, ATerm),
    current_output(CO),
    set_output(S),
    writeq(ATerm),
    write('.'),
    nl,
    set_output(CO).
%       portray_clause(S, ATerm).

unittest_print_clauses(Term, S, Dict) :-
    current_output(CO),
    set_output(S),
    maplist(unittest_print_clause(S, Dict), Term),
    set_output(CO).
