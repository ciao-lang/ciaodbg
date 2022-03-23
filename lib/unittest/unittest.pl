:- module(unittest, [], [assertions, regtypes, isomodes, nativeprops, dcg, fsyntax, hiord, datafacts, define_flag]).

:- doc(title, "Unit testing").

:- doc(author, "Edison Mera").
:- doc(author, "Pedro L@'{o}pez").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Alvaro Sevilla San Mateo").
:- doc(author, "Nataliia Stulova").
:- doc(author, "Ignacio Casso").

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

   @item @var{Golbal properties} specifies some global properties that
   the predicate should meet, in the same way as other assertions. For
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
   are provided in module @lib{unittest_props.pl} (see @ref{Special
   properties for testing}). For example, the limit to the number of
   solutions to be generated for the tested predicate can be set with
   the property @code{try_sols(N)}, a timeout to a test can be set
   with the property @code{timeout(N)}, @code{times(N)} specifies that
   the given test should be executed @code{N} times, etc.

@subsection{Unit tests as examples}

   The special property @tt{example} can be used to mark the unit test
   as an example, so that it is documented as such in manuals. The
   default behavior in @apl{lpdoc} is to not include the unit tests in
   manuals unless they are marked this way. For example, the following
   test would be included in the manual as an example:

@begin{verbatim}
:- test ceval(A, B) : (A = c(3, 4) + c(1, 2) - c(2, 3))
    => (B = c(2, 3)) + (not_fails, is_det, example).
@end{verbatim}

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

:- use_module(engine(stream_basic)).
:- use_module(library(streams), [nl/0]).
:- use_module(library(stream_utils), [write_string/1, file_to_string/2]).
:- use_module(engine(messages_basic), [message/2, messages/1]).
:- use_module(library(unittest/unittest_statistics), [statistical_summary/1]).
:- use_module(library(terms),      [atom_concat/2]).
:- use_module(library(sort),       [sort/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(assertions/assrt_lib), [
    cleanup_code_and_related_assertions/0,
    assertion_read/9,
    clause_read/7,
    get_code_and_related_assertions/5,
    assertion_body/7
]).
:- use_module(library(system),   [copy_file/2, file_exists/1]).
:- use_module(library(hiordlib), [maplist/2]).
:- use_module(library(compiler/c_itf), [exports/5, defines_module/2, module_from_base/2]).
:- use_module(library(bundle/bundle_paths), [bundle_shorten_path/2]).
:- use_module(library(lists), [
    member/2,
    append/3,
    length/2,
    select/3,
    intersection/3,
    difference/3
]).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(compiler/exemaker), [make_exec/2]).
:- use_module(library(hiordlib), [maplist/3]).
:- use_module(engine(internals), [opt_suff/1]).

:- use_module(library(unittest/unittest_base), [
    make_test_id/5,
    runner_global_file_name/1,
    tmp_dir/1,
    wrapper_file_name/3,
    yesno/1,
    read_data/2,
    write_data/2,
    get_stdout_redirection_file/2,
    get_stderr_redirection_file/2
]).
:- use_module(library(unittest/unittest_utils),[assert_from_file/2]).
:- use_module(library(source_tree), [
    current_file_find/3,
    remove_dir/1
]).
:- use_module(library(pathnames), [pathname/1]).
:- use_module(library(messages),  [note_message/2]).
:- use_module(library(formulae),  [list_to_conj/2]).

:- use_module(library(unittest/unittest_db)).
:- use_module(library(unittest/unittest_regression), [
    save_output/2,
    brief_compare/3,
    compare/2,
    test_description/6 % move somewhere else
]).

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
    cleanup_test_results,
    cleanup_global_runners(TestRunDir).

:- pred cleanup_global_runners(TestRunDir) : pathname(TestRunDir).
cleanup_global_runners(TestRunDir) :-
    % tests directory must exist
    ( file_exists(TestRunDir)
    -> file_runtest_input(TestRunDir, InputFile),
       % tests directory must always contain input files
       ( file_exists(InputFile)
       -> remove_dir(TestRunDir)
       ; note_message("~w does not look like a tests source directory, aborting.",[TestRunDir]),
         throw(error(existence_error(source_sink,InputFile), cleanup_global_runners/1-1))
       )
    ; true % nothing to clean
    ).

% ----------------------------------------------------------------------

:- export(show_untested_exp_preds/1).
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
    current_fact(opt_suff(Suff)),
    absolute_file_name(Alias, Suff, '.pl', '.', FileName, FileBase,
         _AbsDir),
    module_from_base(FileBase, Module),
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

:- export(run_tests_in_dir_rec/2).
:- pred run_tests_in_dir_rec(BaseDir, Opts) : pathname * list(test_option)
# "Executes all the tests in the modules of the given directory and
   its subdirectories. You can indicate that the modules in a
   sub-directory should not be tested by placing an empty NOTEST file
   in that sub-directory.  Also, if a NOTESTFILES file is present,
   containing patterns for modules, those modules will not be tested.".

run_tests_in_dir_rec(BaseDir, Opts) :-
    run_tests(BaseDir, [dir_rec | Opts], [check, show_output, show_stats]).

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

:- export(test_option/1).
:- regtype test_option(Opt) # "@var{Opt} is a testing option.".

test_option := rtc_entry.
test_option := stdout(~test_std_option).
test_option := stderr(~test_std_option).
test_option := dump_output | dump_error. % backwards compatible, equivalent to save+show
test_option := treat_related | dir_rec.

% handling of standard stream for testing
test_std_option := show % show
                 | save % save (for regression). Default
                 | null % throw away
                 | stdout. % redirect to stdout (only for stderr)

% TODO: missing support for redirecting stdin(_)?

:- export(test_action/1).
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

:- export(show_test_output/2).
:- pred show_test_output(Alias, Format) : (sourcename(Alias), atm(Format))
    # "Given a file @var{Alias}, tries to look up the respective
      unittest output file and print it to the standard output in
      the specified @var{Format} ('output' for test full trace, 'stats'
      for a statistical summary only, 'full' for both), otherwise emits
      a warning message that no test output file is avaiable.".
% TODO: it is false that it emits a warning message if there is no 
% output

show_test_output(Alias, Format) :-
    show_test_output_(Format, Alias, []).

:- export(show_test_related_output/2).
show_test_related_output(Alias, Format) :-
    show_test_output_(Format, Alias, [treat_related]).

show_test_output_(output, Alias, Opts) :-
    run_tests(Alias, Opts, [show_output]).
show_test_output_(stats, Alias, Opts) :-
    run_tests(Alias, Opts, [show_stats]).
show_test_output_(full, Alias, Opts) :-
    run_tests(Alias, Opts, [show_output, show_stats]).


show_test_output_format(output, TestResults) :-
    show_test_summaries(TestResults).
show_test_output_format(stats, TestResults) :-
    statistical_summary(TestResults).
show_test_output_format(full, TestResults) :-
    show_test_summaries(TestResults),
    statistical_summary(TestResults).

:- reexport(library(unittest/unittest_summaries), [show_test_summaries/1]).

:- reexport(library(unittest/unittest_statistics), [statistical_summary/1, get_statistical_summary/2]).

% ----------------------------------------------------------------------

:- multifile test_filter/2.
:- use_module(library(unittest/unittest_filters), []). % to import multifile test_filter/2

filter_with_options([],_).
filter_with_options([filter(Filter)|Opts], Test) :- !,
    test_filter(Filter, Test),
    filter_with_options(Opts, Test).
filter_with_options([_|Opts], Test) :-
    filter_with_options(Opts, Test).

:- export(run_tests/3).
run_tests(Target, Opt, Actions) :-
    run_tests(Target, Opt, Actions, filter_with_options(Opt)).
% TODO: filters are implemented with metapredicates but passed as
% options because the original design was to pass them as higher order
% arguments too. Now that they are passed as options, we should get
% rid of meta_predicates to handle this everywhere
:- use_module(library(streams)).

:- export(run_tests/4).
:- meta_predicate run_tests(?,?,?,pred(1)).
run_tests(Target, Opts0, Actions, Filter) :- % TODO: ensure Opts and Actions are valid
    process_options(Opts0, Opts),
    cleanup_modules_under_test,
    decide_modules_to_test(Target, Opts, Modules0),
    read_tests(Modules0, Modules),
    % run the tests
    ( member(check, Actions) ->
        run_tests_in_all_modules(Modules, Filter, Opts)
    ; true
    ),
    % show tested predicates' output
    ( member(show_stdout, Actions) ->
        show_stdout(Modules, Filter, new) % shows stdout if it is saved in output file
    ; true
    ),
    % show tested predicates' error
    ( member(show_stderr, Actions) -> % shows stderr if it is saved in output file
        show_stderr(Modules, Filter, new)
    ; true
    ),
    % show_test results
    ( member(show_output, Actions) ->
        get_all_test_outputs(Modules, Filter, new, TestResults),
        show_test_output_format(output, TestResults)
    ; true
    ),
    % show test statistics
    ( member(show_stats, Actions) ->
        get_all_test_outputs(Modules, Filter, new, TestResults),
        show_test_output_format(stats, TestResults)
    ; true
    ),
    ( member(save, Actions) ->
        save_output(Modules, Filter)
    ; true
    ),
    ( member(briefcompare(ReturnStatus), Actions) ->
        brief_compare(Modules, Filter, ReturnStatus)
    ; true
    ),
    ( member(briefcompare, Actions) ->
        brief_compare(Modules, Filter, _)
    ; true
    ),
    ( member(compare, Actions) ->
        compare(Modules, Filter)
    ; true
    ),
    ( member(show_saved_stdout, Actions) ->
        show_stdout(Modules, Filter, saved)
    ; true
    ),
    ( member(show_saved_stderr, Actions) ->
        show_stderr(Modules, Filter, saved)
    ; true
    ),
    ( member(show_saved_output, Actions) ->
        get_all_test_outputs(Modules, Filter, saved, TestResults),
        show_test_output_format(output, TestResults)
    ; true
    ),
    ( member(show_saved_stats, Actions) ->
        get_all_test_outputs(Modules, Filter, saved, TestResults),
        show_test_output_format(output, TestResults)
    ; true
    ),
    ( member(status(TestStatus), Actions) ->
        get_all_test_outputs(Modules, Filter, new, TestResults),
        get_statistical_summary(TestResults, Stats),
        Stats=stats(NTotal,NSuccess,_,_,_,_,_),
        ( NTotal=NSuccess ->
            TestStatus=0
        ;
            TestStatus=1
        )
    ; true
    ).


decide_modules_to_test(Target, Opts, Modules) :-
    assert_modules_to_test(Target, Opts),
    findall(Path-Module,
            (module_base_path_db(Module,_,Path)),
            PathsModules0),
    sort(PathsModules0, PathsModules), % because order is not stable across different intallations (e.g., mine (IC) and Gitlab's)
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
          get_related_modules(Path,RelatedModules), % TODO: handle syntactic errors in get_code_and_related_assertions/5
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

:- meta_predicate run_tests_in_all_modules(?,pred(1),?).
run_tests_in_all_modules([], _, _).
run_tests_in_all_modules([Module|Modules], Filter, Opts) :-
    run_tests_in_one_module(Module, Filter, Opts),
    run_tests_in_all_modules(Modules, Filter, Opts).
% TODO: option for running tests for more than one module at once

:- meta_predicate run_tests_in_one_module(?,?,pred(1),?).
run_tests_in_one_module(Module, Filter, Opts) :-
    tmp_dir(TestRunDir),
    cleanup_unittest(TestRunDir), % TODO: check if module is already read before cleaning and reading again
    module_base_path_db(Module,_,Path),
    get_assertion_info(current,Path,[Module]),
    run_test_assertions(TestRunDir, [Module], Filter, Opts).

:- meta_predicate show_stdout(?,pred(1),?).
show_stdout([], _, _).
show_stdout([Module|Modules], Filter, WhichOutput) :-
    show_stdout_(Module, Filter, WhichOutput),
    show_stdout(Modules, Filter, WhichOutput).

:- meta_predicate show_stdout_(?,pred(1),?).
show_stdout_(Module, Filter, WhichOutput) :-
    % TODO: why not 'cleanup_test_attributes'? (JF)
    test_input_file_to_test_attributes_db(Module),
    filter_test_attributes_db(Filter),
    cleanup_test_results,
    load_test_output(Module, WhichOutput),
    ( % (failure-driven) loop
        retract_fact(test_attributes_db(TestId,_,F,A,_,_,Body,loc(_,LB,LE))),
        assertion_body(_,_,_,_,_,Comment,Body),
        retract_fact(test_output_error_db(TestId,Output,_)),
          ( Output=[_|_] -> % as opposed to empty, 'dumped', or 'ignored'. % TODO: messages for those
              test_description(F,A,Comment,LB,LE,TestMsg),
              messages([message(note, ['Output in test ', [](TestMsg), ':', '\n'])]),
              write_string(Output)
          ; true
          ),
        fail
    ;
        true
    ).

:- meta_predicate show_stderr(?,pred(1),?).
show_stderr([],_,_).
show_stderr([Module|Modules], Filter, WhichOutput) :-
    show_stderr_(Module, Filter, WhichOutput),
    show_stderr(Modules, Filter, WhichOutput).
% TODO: unify with show_stdout

:- meta_predicate show_stderr_(?,pred(1),?).
show_stderr_(Module, Filter, WhichOutput) :-
    % TODO: why not 'cleanup_test_attributes'? (JF)
    test_input_file_to_test_attributes_db(Module),
    filter_test_attributes_db(Filter),
    cleanup_test_results,
    load_test_output(Module, WhichOutput),
    ( % (failure-driven) loop
        retract_fact(test_attributes_db(TestId,_,F,A,_,_,Body,loc(_,LB,LE))),
        assertion_body(_,_,_,_,_,Comment,Body),
        retract_fact(test_output_error_db(TestId,_,Error)),
          ( Error=[_|_] ->  % as opposed to empty, 'dumped', 'redirected_to_output' or 'ignored'. % TODO: messages for those
              test_description(F,A,Comment,LB,LE,TestMsg),
              messages([message(note, ['Error in test ', [](TestMsg), ':', '\n'])]),
              write_string(Error)
          ; true
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

:- export(run_tests_in_module/3).
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
    get_all_test_outputs(Modules, filter_with_options(Opts), new, TestSummaries).

:- export(run_tests_in_module/2).
:- pred run_tests_in_module(Alias, Opts)
    : (sourcename(Alias), list(test_option,Opts))
# "Run the tests in module @var{Alias}. The results of the tests are
   printed out.".
run_tests_in_module(Alias, Opts) :-
     run_tests(Alias, Opts, [check, show_output, show_stats]).

:- export(run_tests_in_module/1).
:- pred run_tests_in_module(Alias) : sourcename(Alias)
# "Run the tests in module @var{Alias} (using default options).  The
   results of the tests are printed out.".

run_tests_in_module(Alias) :-
    run_tests(Alias, [], [check, show_output, show_stats]).

:- export(run_tests_in_module_check_exp_assrts/1).
run_tests_in_module_check_exp_assrts(Alias) :-
    run_tests(Alias, [rtc_entry], [check, show_output, show_stats]).

:- export(run_tests_related_modules/1).
:- pred run_tests_related_modules(Alias) : sourcename(Alias).

run_tests_related_modules(Alias) :-
    run_tests(Alias, [treat_related], [check, show_output, show_stats]).

% ----------------------------------------------------------------------

:- use_module(engine(internals), [opt_suff/1]).

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
    current_fact(opt_suff(Suff)),
    absolute_file_name(Alias, Suff, '.pl', '.', FileName, Base, AbsDir),
    module_from_base(Base, Module),
    get_code_and_related_assertions(FileName, Module, Base, '.pl', AbsDir). % TODO: handle syntactic erros
get_assertion_info(related, Alias, Modules) :-
    absolute_file_name(Alias, FileName),
    get_code_and_related_assertions(FileName, _, _, _, _), % TODO: handle syntactic erros
    set_of_modules(Modules).

set_of_modules := ~sort(~findall(Module, current_assr_module(Module))).

current_assr_module(Module) :-
    assertion_read(_A, Module, check, Type, _E, _F, _G, _H, _I),
    unittest_type(Type).

% ---------------------------------------------------------------------------

:- compilation_fact(unittest_runner_process).

:- if(defined(unittest_runner_process)).
% Separate runner process (more robust)

:- use_module(ciaobld(config_common), [libcmd_path/4]).
unittest_exec := ~libcmd_path(ciaodbg, plexe, 'unittest_runner').

:- use_module(ciaobld(cpx_process), [cpx_process_call/3]).
invoke_unittest(Args, Status) :-
    % TODO: run on background? (process file_runtest_output as it is written)
    cpx_process_call(~unittest_exec, Args, [
        stdin(null),
        stdout(default), % dumping/saving/ignoring output options handled in runner
        stderr(default), % dumping/saving/ignoring/redirecting error options handled in runner
        status(Status)
    ]).

:- else. % \+defined(unittest_runner_process)
% Same process (less robust)

:- use_module(library(unittest/unittest_runner), [main/1]).
invoke_unittest(Args, Status) :- unittest_runner:main(Args), Status=0.

:- endif.

% ---------------------------------------------------------------------------

:- pred run_test_assertions(+pathname, +list(atm), +cgoal, +list) +
    (not_fails, no_choicepoints).

:- meta_predicate run_test_assertions(?,?,pred(1),?).
run_test_assertions(TestRunDir, Modules, Filter, Opts) :-
    mkpath(TestRunDir),
    create_test_input(TestRunDir, Modules, Filter), % asserts and writes in input file test_attributes_db/8 facts
    % Even if module has no tests, we write an empty test output file
    % in order to mark that testing had been performed on a module,
    % which allows detecting newly added tests on regression testing
    write_all_test_outputs(Modules), % (Initial empty outputs)
    %
    ( test_attributes_db(_, _, _, _, _, _, _, _) ->
      get_test_opt(rtc_entry, RtcEntry, Opts),
      create_wrapper_mods(Modules, TestRunDir, RtcEntry, WrapperMods),
      do_tests(TestRunDir, Modules, WrapperMods, Opts)
    ; true
    ),
    write_all_test_outputs(Modules). % TODO: mark them as finished?

% even if module has no tests, we write an empty test output file
% in order to mark that testing had been performed on a module,
% which allows detecting newly added tests on regression testing
write_all_test_outputs([]).
write_all_test_outputs([Module|Mods]) :-
    test_results_db_to_file_test_output(Module),
    write_all_test_outputs(Mods).

:- meta_predicate get_all_test_outputs(?,pred(1),?,?).
get_all_test_outputs([], _, _, []).
get_all_test_outputs([Module|Modules], Filter, WhichOutput, [TestResult|TestResults]) :-
    get_module_output(Module, Filter, WhichOutput, TestResult),
    get_all_test_outputs(Modules, Filter, WhichOutput, TestResults).

:- meta_predicate get_module_output(?,pred(1),?,?).
get_module_output(Module, Filter, WhichOutput, TestResult) :-
    cleanup_test_attributes,
    test_input_file_to_test_attributes_db(Module),
    filter_test_attributes_db(Filter),
    cleanup_test_results,
    load_test_output(Module, WhichOutput),
    mark_missing_as_aborted(Module),
    findall(IdxTestSummary,
            get_one_test_assertion_output(Module, IdxTestSummary),
            TestResult).

load_test_output(Module, WhichOutput) :-
    ( WhichOutput = new ->
        file_test_output_to_test_results_db(Module)
    ; file_saved_test_output_to_test_results_db(Module)
    ).

:- pred get_one_test_assertion_output(Module, IdxTestSummary)
    :  atm(Module)
    => struct(IdxTestSummary).

get_one_test_assertion_output(Module, TestAttributes-TestResults) :-
    test_attributes_db(TestId, Module, F, A, Dict, _, Body, loc(Source, LB, LE)),
    assertion_body(_,_,_,_,_,Comment,Body),
    findall(TestResult, test_output_db(TestId, TestResult), TestResults),
    TestAttributes = test_attributes(Module, F, A, Dict, Comment,
                                     Source, LB, LE).

:- pred do_tests(TestRunDir, Modules, WrapperMods, Opts)
    :  pathname * list(atom) * list * list(test_option)
# "Calls the runner as an external process. If some test aborts, calls
   recursively with the rest of the tests".
do_tests(TestRunDir, Modules, WrapperMods, Opts) :-
    do_tests_(first, TestRunDir, Modules, WrapperMods, Opts).

do_tests_(Resume, TestRunDir, Modules, WrapperMods, Opts) :-
    current_prolog_flag(unittest_default_timeout,TimeoutN),
    atom_number(TimeoutAtm,TimeoutN),
    RunnerArgs1 = [timeout,TimeoutAtm |Opts],
    ( Resume = resume_after(ContIdx) ->
        RunnerArgs2 = [resume_after,ContIdx |RunnerArgs1]
    ; RunnerArgs2 = RunnerArgs1
    ),
    ( opt_suff(Suff) ->
        RunnerArgs3 = [suff, Suff | RunnerArgs2]
    ; RunnerArgs3 = RunnerArgs2
    ),
    append(['--begin_wrapper_modules--' | WrapperMods], ['--end_wrapper_modules--' | RunnerArgs3], RunnerArgs4),
    RunnerArgs = [dir, TestRunDir | RunnerArgs4],
    % Cleanup runtest output file and call the runner
    runtest_output_file_reset(TestRunDir),
    invoke_unittest(RunnerArgs, Status),
    %
    (Status==101 -> % returned by unittest_runner.pl when a wrapper module does not compile
        message(error, ['Compilation failed. Please make sure all relevant predicates',
                        ' and properties for testing are exported.'])
        % Note: Syntactic compilation errors of the modules under test
        % are detected earlier, in the call to
        % get_code_and_related_assertions/5 in the predicate
        % get_assertion_info/3.
        %
        % TODO: write aborted results for each tests or abort
        % completely. Right now it prints the messages but it shows
        % the tests as passed in the stadistics
        %
        % TODO: save compilation status in test.out
        %
        % TODO: unique return status for run_tests_in_module/3
    ;
        runtest_output_file_to_test_results_db(TestRunDir),
        write_all_test_outputs(Modules), % TODO: make incremental! only for new results
        %
        obtain_test_cont(TestRunDir, Opts, Cont),
        ( Cont = yes(TestId) -> % continue
            do_tests_(resume_after(TestId), TestRunDir, Modules, WrapperMods, Opts)
        ; true % done
        )
    ).

obtain_test_cont(TestRunDir, Opts, Cont) :-
    ( retract_fact(test_output_event(continue_after(TestId))) ->
        % Continue event (e.g., a runner restart)
        Cont = yes(TestId)
    ; missing_output_test(TestId,_Module) ->
        % No continue event and there are tests without output, assume
        % that they were aborted and try to recover its output
        % TODO: have a less of a hack mechanism to detect this
        recover_aborted_test(TestId, TestRunDir, Opts),
        Cont = yes(TestId)
    ; Cont = no
    ).

% First aborted test (test without output)
missing_output_test(TestId,Module) :- % (nondet)
    test_attributes_db(TestId,Module,_,_,_,_,_,_),
    \+ test_output_error_db(TestId,_,_).

:- use_module(library(unittest/unittest_runner), [
    get_stdout/3,
    get_stderr/3,
    get_stdout_option/2,
    get_stderr_option/2]).

% TODO: unify with unittest_runner properly
recover_aborted_test(TestId, TestRunDir, Options) :-
    % recover and possibly dump stdout until crash
    get_stdout_option(Options, OutputMode),
    get_stdout(OutputMode, TestRunDir, StdoutStr),
    % recover and possibly dump stderr until crash
    get_stderr_option(Options, ErrorMode),
    get_stderr(ErrorMode, TestRunDir, StderrStr),
    % mark the test as aborted
    fill_aborted_test(TestId, StdoutStr, StderrStr).
% (do not mark in file!)
%    open(OutFile, append, IO),
%    TestResult = st(unknown, [], [], aborted(StdoutStr, StderrStr)),
%    write_data(IO, test_output_db(TestId, TestResult)),
%    write_data(IO, test_output_error_db(TestId, StdoutStr, StderrStr)),
%    close(IO).
% TODO: unify with unittest_runner.pl

fill_aborted_test(TestId, StdoutStr, StderrStr) :-
    TestResult = st(unknown, [], [], aborted(StdoutStr, StderrStr)),
    assertz_fact(test_output_db(TestId, TestResult)),
    assertz_fact(test_output_error_db(TestId, StdoutStr, StderrStr)).

% Mark all tests with missing output as aborted
mark_missing_as_aborted(Module) :-
    ( % (failure-driven loop)
      missing_output_test(TestId,Module),
      \+ test_output_error_db(TestId,_Output,_Error),
        fill_aborted_test(TestId, "", ""),
        fail
    ; true
    ).

% creates or updates .testin files for each module
read_tests([],[]).
read_tests([M|Modules0],[M|Modules]) :-
    read_tests_in_module(M), !,
    read_tests(Modules0,Modules).
read_tests([_|Modules0],Modules) :-
    read_tests(Modules0,Modules).

read_tests_in_module(Module) :-
    file_test_input(Module,_TestIn),
    fail, !. % TODO: exists and its modification date is later than Module's
read_tests_in_module(Module) :-
    create_input_file(Module).
% TODO: do it right with itfs. Ask Jose

:- data compilation_error/0.

create_input_file(Module) :-
    module_base_path_db(Module,Base,Path),
    cleanup_code_and_related_assertions, % TODO: check if last get_code_and_related_assertion read this module
    retractall_fact(compilation_error),
    intercept(
        get_code_and_related_assertions(Path, Module, Base, '.pl', _AbsDir),
        compilation_error,
        assertz_fact(compilation_error) % just "fail" or "!, fail" does not work. Why? Spurious choicepoints in get_code_and_related_assertions/5?
    ),
    (retract_fact(compilation_error) -> !, fail ; true), % message?
    file_test_input(Module,TestIn),
    open(TestIn, write, SI),
    ( % (failure-driven loop)
        get_test(Module, TestId, Type, Pred, Body0, Dict, Src0, LB, LE),
        bundle_shorten_path(Src0,Src),
        assertion_body(Pred,Compat,Calls,Succ,Comp0,Comment,Body0),
        intersection(Comp0, ~valid_texec_comp_props, TestOptions0),
        difference(Comp0, ~valid_texec_comp_props, Comp),
        texec_warning(Type, Comp, Pred, asrloc(loc(Src,LB,LE))),
        assertion_body(Pred,Compat,Calls,Succ,Comp,Comment,Body),
        functor(Pred, F, A),
        get_test_options(TestOptions0,Base,TestOptions),
        assertz_fact(test_attributes_db(TestId, Module, F, A, Dict, TestOptions,
                Body, loc(Src, LB, LE))),
        write_data(SI, test_attributes_db(TestId, Module, F, A, Dict, TestOptions,
                Body, loc(Src, LB, LE))),
        fail
    ;
        close(SI)
    ).
% TODO: create a temporary module that imports all modules and do
% get_code_and_related_assertions of that module only once

:- pred create_test_input(+pathname, +list(atm), +cgoal) + (not_fails, no_choicepoints).

% asserts test attributes in test_attributes_db/n and writes them in
% runner input file
:- meta_predicate create_test_input(?,?,pred(1)).
create_test_input(TestRunDir, Modules, Filter) :-
    cleanup_test_attributes,
    ( % (failure-driven loop)
        member(Module, Modules), % backtracking here
          test_input_file_to_test_attributes_db(Module),
        fail
    ;
        filter_test_attributes_db(Filter),
        test_attributes_db_to_runtest_input_file(TestRunDir)
    ).
% what was the purpose of this?
%% atom_concat(FileTestInput, '.bak', FileTestInput0),
%% copy_file(FileTestInput, FileTestInput0).

get_test_options(Options, _, Options) :-
    member(timeout(_,_), Options), !.
get_test_options(Options, ModuleBase, [timeout(_,Timeout)|Options]) :-
    clause_read(ModuleBase, 1, unittest_default_timeout(Timeout), _, _, _, _), !.
get_test_options(Options, _, Options).

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
    module_base_path_db(Module,Base,_),
    ( wrapper_file_name(TestRunDir, Module, WrapperFile),
      create_module_wrapper(Module, RtcEntry, Base, WrapperFile)
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
create_module_wrapper(Module, RtcEntry, Src, WrapperFile) :-
    Header = [
            (:- module(_, _, [assertions, nativeprops, rtchecks])),
            (:- use_module(library(unittest/unittest_runner_aux))),
            (:- use_module(library(rtchecks/rtchecks_rt))),
            (:- use_module(library(rtchecks/rtchecks_basic))),
            (:- use_module(library(unittest/unittest_props))),
            (:- use_module(Src)),
            (:- discontiguous test_entry/3),
            (:- multifile test_entry/3),
            (:- discontiguous test_check_pred/3),
            (:- multifile test_check_pred/3)
    ],
    collect_test_modules(Src, TestModules),
    % here link the TestEntry clause with the ARef test identifier
    findall(Cls,
            gen_test_entry(Module, RtcEntry, Src, Cls),
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

% TODO: tests abort when the predicate is not defined in the module,
%       fix?  Depends on if we allow tests for imports -- otherwise
%       this code is still useful for writing test assertions of
%       impldef preds such as foreign
gen_test_entry(Module, RtcEntry, Src, Clauses) :-
    % get current test assertion
    % *** get_test(Module, TestId, AType, Pred, ABody, ADict, ASource, ALB, ALE), % TODO: use test_attributes_db, filter tests
    test_attributes_db(TestId, Module, _, _, ADict, _, ABody, loc(ASource, ALB, ALE)),
    assertion_body(Pred,_,Calls,_,_,_,ABody),
    TestInfo = testinfo(Pred, ABody, ADict, ASource, ALB, ALE),
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
    ( clause_read(Src, Head, _, _, PSource0, PLB, PLE) ->
        bundle_shorten_path(PSource0,PSource),
        PLoc = loc(PSource, PLB, PLE)
    ; PLoc = none
    ),
    % this term is later expanded in the wrapper file by the goal
    % translation of the rtchecks package
    TestBody = '$check_pred_body'(TestInfo, Assertions, PLoc),
    Clauses = [
        clause(test_check_pred(Module, TestId, Pred), TestBody, ADict),
        clause(test_entry(Module,TestId,Pred), ~list_to_conj(Calls), ADict)
    ].



:- pop_prolog_flag(write_strings).

valid_texec_comp_props([times(_, _), try_sols(_, _), generate_from_calls_n(_,_), timeout(_,_)]).

:- pred texec_warning(AType, GPProps, Pred, AsrLoc)
    : (atm(AType), list(GPProps), term(Pred), struct(AsrLoc))
  # "A @tt{texec} assertion cannot contain any computational properties
    except @pred{times/2} and @pred{try_sols/2}.  So if a @tt{texec}
    assertion contains any other computational property the corresponding
    test will be generated as if the assertion was of the type
    @tt{test}. The user is notified of this fact by a warning message.
    @var{AType} takes values @tt{texec} and @tt{test}, @var{GPProps}
    is a list of non-ground comp properties, @var{Pred} is a predicate
    from the test assertion and @var{AsrLoc} is the locator of the
    test assertion.".

texec_warning(texec, GPProps, Pred, asrloc(loc(ASource, ALB, ALE))) :-
    \+ GPProps == [], !,
    functor(Pred, F, A),
    maplist(comp_prop_to_name, GPProps, GPNames),
    Message = message_lns(ASource, ALB, ALE, warning,
            ['texec assertion for ', F, '/', A,
             ' can have only unit test commands, ',
             'not comp properties: \n', ''(GPNames),
             '\nProcessing it as a test assertion']),
    messages([Message]). % TODO: use message/2?
texec_warning(_, _, _, _).

comp_prop_to_name(C0, C) :- C0 =.. [F, _|A], C =.. [F|A].


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
%       write(' .'),
%       nl,
%       set_output(CO).

unittest_print_clause(S, Dict, Term) :-
    apply_dict(Term, Dict, ATerm),
    current_output(CO),
    set_output(S),
    writeq(ATerm),
    write(' .'),
    nl,
    set_output(CO).
%       portray_clause(S, ATerm).

unittest_print_clauses(Term, S, Dict) :-
    current_output(CO),
    set_output(S),
    maplist(unittest_print_clause(S, Dict), Term),
    set_output(CO).


% --------------------------------
% Unittest options
% -------------------------------

process_options([],[]).
process_options([Opt0|Opts0],[Opt|Opts]) :-
    nonvar(Opt0),
    test_option(Opt0), !,
    process_option(Opt0,Opt),
    process_options(Opts0,Opts).
process_options([Opt|Opts0],Opts) :-
    message(warning, ['Unknown unittest option ', Opt, '.\n']),
    process_options(Opts0,Opts).

% options are passed down as runner args, which must be atoms
process_option(stdout(show), dump_output_real_time).
process_option(stdout(save), save_output).
process_option(stdout(null), ignore_output).
process_option(stderr(show), dump_error_real_time).
process_option(stderr(save), save_error).
process_option(stderr(null), ignore_error).
process_option(stderr(stdout), error_to_output).
process_option(Opt, Opt).
