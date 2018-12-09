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
	    [assertions, regtypes, isomodes, nativeprops, dcg, fsyntax, hiord]).

:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic), [nl/0]).
:- use_module(engine(messages_basic), [display_string/1]).
:- use_module(engine(messages_basic), [message/2]).
:- use_module(library(unittest/unittest_statistics), [statistical_summary/2]).
:- use_module(library(terms),      [atom_concat/2]).
:- use_module(library(sort),       [sort/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(rtchecks/rtchecks_pretty),
        [
            pretty_messages/1,
            pretty_prop/3,
            rtcheck_to_messages/3
        ]).
:- use_module(library(assertions/assrt_lib),
        [
            cleanup_code_and_related_assertions/0,
            assertion_read/9,
            clause_read/7,
            get_code_and_related_assertions/5,
            assertion_body/7
        ]).
:- use_module(library(assertions/c_itf_props), [filename/1]).
:- use_module(library(system),   [copy_file/2, file_exists/1]).
:- use_module(library(process),  [process_call/3]).
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
            write_data/2
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

:- doc(title, "Unit testing").

:- doc(author, "Edison Mera").
:- doc(author, "Pedro L@'{o}pez").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Alvaro Sevilla San Mateo").
:- doc(author, "Nataliia Stulova").

:- doc(summary, "Ciao provides an assertion-based testing
   functionality, including unit tests.  The central idea is to use
   the assertion language to provide specifications of test cases for
   a given predicate. This library contains predicates that can be
   used to run tests in modules and gather or pretty print the
   results.").

:- doc(module, "The Ciao assertion language (see @ref{The Ciao
   assertion language}) allows writing @index{tests} (including
   @index{unit tests}) by means of @index{test assertions}. These
   assertions make it possible to write specific test cases at the
   predicate level. This library contains predicates that can be used
   to run tests in modules and gather or pretty print the results. It
   also provides some special properties that are convenient when
   writing tests and the required run-time support.

@subsection{Writing test assertions}
@cindex{writing unit tests}

   Recall that a @index{test assertion} is written as follows:

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
   somewhat more operational (``test oriented''), reading to these
   fields: 

   @begin{itemize}
   @item @pred{predicate/n} is the predicate to be tested.

   @item @var{Precondition} is a goal that is called before the
   predicate being tested, and can be used to generate values of the
   input parameters. While in other types of assertions the idea of
   these @em{preconditions} is to provide concrete input values for
   which the predicate can be actually executed.

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

   Due to the non-determinism of logic programs, the test engine needs
   to test all the solutions that can be tested up to given limits
   (for example, a maximum number of solutions, or a given timeout).

   There are some specific properties that only apply to testing which
   are provided in module @lib{unittest_props.pl}. For example
   @code{try_sols(N)} specifies that the first N solutions of the
   predicate @code{predicate/n} are tested.  @code{times(N)} specifies
   that the given test should be executed N times, etc.

@subsection{Running tests in a bundle}

   To run all these tests (as well as the other standard tests in the
   system) run the following (at the top level of the source tree or a
   bundle @cindex{bundle}):

@begin{verbatim}
ciao test
@end{verbatim}

@subsection{Running the tests from the IDE}

   A convenient way to run these tests is by selecting options in the
   @bf{CiaoDbg menu within the development environment}:
   @cindex{running unit tests}

@begin{cartouche}
   @begin{enumerate}

   @item @tt{Run tests in current module}: execute only the tests
         specified in the current module.

   @item @tt{Run tests in current and all related modules}: execute
         the tests specified in the module and in all the modules
         being used by this.

   @item @tt{Show untested exported predicates}: show the
         @em{exported} predicates that do not have any test assertion.

   @end{enumerate}
@end{cartouche}

@subsection{Running the tests from the top level or programmatically}

   The tests can also be run from the top level, loading this module
   (@lib{unittest.pl}) and calling the appropiate predicates that it
   exports (see the module @bf{Usage and Interface} section
   below). This can also be done from a program, provided it imports
   this module.

@subsection{Integration tests}

If you need to write tests for predicates that are spread over several
modules, but work together, it may be useful to create a separate
module, and reexport the predicates required to build the tests. This
allows performing @em{integration testing}, using the same syntax of
the test assertions.
").

:- doc(bug, "The tests currently @bf{can only be applied to exported
   predicates}.  This is a limitation of the current implementation
   that will be corrected in the future.").

:- push_prolog_flag(write_strings, on).

%% put to unittest_base together with other file names?
loader_name('ciao_unittest_loader').

% ----------------------------------------------------------------------

:- pred cleanup_unittest(TmpDir) : pathname(TmpDir).
cleanup_unittest(TmpDir) :-
	cleanup_code_and_related_assertions,
	cleanup_test_attributes,
	cleanup_global_runners(TmpDir).

cleanup_test_attributes :-
	retractall_fact(test_attributes_db(_, _, _, _, _, _, _, _, _)).

:- pred cleanup_global_runners(TmpDir) : pathname(TmpDir).
cleanup_global_runners(TmpDir) :-
        % tests directory must exist
        ( file_exists(TmpDir)
        -> file_test_input(InputBase),
           path_concat(TmpDir,InputBase,InputFile),
           % tests directory must always contain input files
           ( file_exists(InputFile)
           -> remove_dir(TmpDir)
           ; note_message("~w does not look like a tests source directory, aborting.",[TmpDir]),
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
	tmp_dir(TmpDir),
	cleanup_unittest(TmpDir),
	get_assertion_info(current, Alias, _Modules),
	findall(Message, current_untested_pred(Alias, Message), Messages),
	pretty_messages(Messages).

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

:- pred test_result_summary(FileName,IdxTestSummaries)
        :: sourcename * list(struct)
# "Database that stores test results as a mapping from an atomic
   @var{FileName} to a list of results of each test in that module
   @var{IdxTestSummaries}. Each element of this list is a pair of the
   format @var{TestAttributes}-@var{TestSummary} where
   @var{TestAttributes} is represented by a term
   test_attributes(Module,Pred,Arity,Dict,Comm,Src,LB,LE), and
   @var{TestSummary} is a list of counter structures of the form
   [count(st(RTCErrors, Signals, Result),TestStatusNumber)]
   where @var{RTCErrors} and @var{Signals} are lists that contain
   information of signals and rtchecks errors, detected while running
   the test, and @var{TestStatusNumber}=1 and is later used in
   the statistical summary preparation.".
% atom @var{FileName} is the same as the @var{Src} one.
:- data test_result_summary/2.

:- pred run_tests_in_dir_rec(BaseDir, Opts) : pathname * list(test_option)
# "Executes all the tests in the modules of the given directory and
   its subdirectories. You can indicate that the modules in a
   sub-directory should not be tested by placing an empty NOTEST file
   in that sub-directory.  Also, is a NOTESTFILES is present
   containing patterns for modules those modules will not be tested.".

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
	      assertions even if the flag runtime_checks has not been activated. (This
	      is a workaround since currently we cannot enable runtime checks in
	      system libraries smoothly).

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
@comment{
% TODO: merge with the regrtest
%        @item @tt{save} : save test results file in @tt{module.testout-saved}
%              file;
%
%        @item @tt{briefcompare} : check whether current and saved test
%              output files differ;
%
%        @item @tt{compare} : see the differences in the current and saved
%              test output files in the diff format;
}
        @end{itemize}").

:- regtype test_action(Action) # "@var{Action} is a testing action".

test_action := check | show_output | show_stats .
% test_action := show_output_short | show_output_full % TODO: verbosity control
% test_action := save | briefcompare | compare . % TODO: merge with regrtest

get_test_opt(Opt, YesNo, Opts) :- member(Opt, Opts), !, YesNo = yes.
get_test_opt(_  , no   , _).

:- pred get_test_opts(-yesno, -yesno, -yesno, +list)
        :  var   * var   * var   * list(test_option)
        => yesno * yesno * yesno * list(test_option).

get_test_opts(DumpOutput, DumpError, RtcEntry, Options) :-
	get_test_opt(dump_output, DumpOutput, Options),
	get_test_opt(dump_error,  DumpError , Options),
	get_test_opt(rtc_entry,   RtcEntry  , Options).

% ----------------------------------------------------------------------

:- pred show_test_output(Alias, Format) : (sourcename(Alias), atm(Format))
        # "Given a file @var{Alias}, tries to lookup the respective
          unittest output file and print it to the standard output in
          the specified @var{Format} ('output' for test full trace, 'stats'
          for a statistical summary only, 'full' for both), otherwise emits
          a warning message that no test output file is avaiable.".

show_test_output(Alias, Format) :-
        show_test_output_(current, Alias, Format).

show_test_related_output(Alias, Format) :-
        show_test_output_(related, Alias, Format).

show_test_output_(TestMode, Alias, Format) :-
        cleanup_code_and_related_assertions,
        get_assertion_info(TestMode, Alias, Modules),
        get_all_test_outputs(Modules, TestResults),
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
	pretty_messages(Messages).

% ----------------------------------------------------------------------

assert_test_results_dir(no , _  , _    ) :- !.
assert_test_results_dir(yes, Dir, Alias) :-
        absolute_file_name(Alias, '_opt', '.pl', '.', _, Base, AbsDir),
        path_split(Base, AbsDir, Module),
        get_module_output(Module, Base, TestResult),
        assertz_fact(test_result_summary(Dir, [TestResult])).

retract_test_results_dir(no , _  ) :- !.
retract_test_results_dir(yes, Dir) :-
        findall(TRs, retract_fact(test_result_summary(Dir, TRs)), TRRs),
        show_test_output_format(stats, TRRs).

run_tests(Dir, Opts0, Actions0) :-
        select(dir_rec, Opts0, Opts),!,
        get_test_opt(show_stats, ShowStats, Actions0),
        ( ShowStats == yes, select(show_stats, Actions0, Actions)
        ; Actions = Actions0
        ),
        ( % (failure-driven loop)
            current_file_find(testable_module, Dir, Alias),
            run_tests(Alias, Opts, Actions),
            assert_test_results_dir(ShowStats, Dir, Alias),
            fail
        ; true
        ),
        retract_test_results_dir(ShowStats, Dir).
run_tests(Alias, Opts, Actions) :-
        tmp_dir(TmpDir),
	cleanup_unittest(TmpDir),
        % choose one of two testing scenarios
        ( member(treat_related, Opts) ->
            TestMode = related
          ; TestMode = current
        ),
        % run the tests
        ( member(check, Actions) ->
          run_tests_in_module_args(TestMode, Alias, Opts)
        ; true
        ),
        % select kind of output to be printed
        ( member(show_output, Actions) ->
          show_test_output_(TestMode, Alias, output)
        ; true
        ),
        ( member(show_stats, Actions) ->
          show_test_output_(TestMode, Alias, stats)
        ; true
        ).

% ----------------------------------------------------------------------

:- pred run_tests_in_module(Alias, Opts, TestSummaries)
    : (sourcename(Alias), list(Opts,test_option))
    => list(TestSummaries)

# "Run the tests in module @var{Alias} (with options @var{Opts}).  The
   results of the tests are returned as data in
   @var{TestSummaries}. @var{TestSummaries} can be pretty printed
   using @pred{show_test_summaries/1} and
   @pred{statistical_summary/2}.".
run_tests_in_module(Alias, Opts, TestSummaries) :-
        run_tests(Alias, Opts, [check]),
        get_assertion_info(current, Alias, Modules),
        get_all_test_outputs(Modules, TestSummaries).

:- pred run_tests_in_module(Alias, Opts)
	: (sourcename(Alias), list(Opts,test_option))

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

run_tests_in_module_args(TestMode, Alias, Opts) :-
	get_assertion_info(TestMode, Alias, Modules),
        tmp_dir(TmpDir),
	run_test_assertions(TmpDir, Modules, Opts).

% ----------------------------------------------------------------------

:- export(get_assertion_info/3).
:- doc(hide,get_assertion_info/3).
:- pred get_assertion_info(TestMode, Alias, Modules)
        : ( atm(TestMode), sourcename(Alias), var(Modules) )
        =>  list(Modules, atm)
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

:- pred create_runner(+pathname, +list, +yesno) + (not_fails, no_choicepoints)
        # "If the list of modules to test @var{Modules} is not empty
           then test runner and test loader files are generated.".

create_runner(TmpDir, Modules, RtcEntry) :-
	( Modules \== [] ->
            % TODO: put together?
	    create_global_runner(TmpDir, Modules, RtcEntry, RunnerFile),
	    create_loader(TmpDir, RunnerFile)
	;
            true
	).

% But note that create_loader/2 cleans the assertion_read/9 database.
% This means that, from this point on, the contents of such predicate
% cannot be trusted.

:- pred create_loader(+pathname, +atm) + (not_fails, no_choicepoints).

create_loader(TmpDir, RunnerFile) :-
	loader_name(BLoader),
        path_concat(TmpDir,BLoader,Loader),
	atom_concat(Loader, '_auto.pl', LoaderPl),
	create_loader_pl(RunnerFile, LoaderPl),
	make_exec([LoaderPl], Loader).

% Kludge: Wrong behavior if you link RunnerFile in the executable directly.
create_loader_pl(RunnerFile, LoaderPo) :-
	Cs = [
                (:- module(_, [main/1], [])),
		(:- use_module(library(compiler), [use_module/1])),
		(main(Args) :- use_module(RunnerFile), _:main_tests(Args))
	    ],
        print_clauses_to_file(LoaderPo, [raw(Cs)]).

:- pred run_test_assertions(+pathname, +list(atm), +list) +
	(not_fails, no_choicepoints).

run_test_assertions(TmpDir, Modules, Opts) :-
	mkpath(TmpDir),
	create_test_input(TmpDir, Modules),
        file_test_input(BInFile),
	path_concat(TmpDir, BInFile, InFile),
        retractall_fact(test_input_db(_, _)),
	assert_from_file(InFile, assert_test_input),
        %
        empty_output(TmpDir),
	( test_attributes_db(_, _, _, _, _, _, _, _, _) ->
          get_test_opts(DumpOutput, DumpError, RtcEntry, Opts),
          create_runner(TmpDir, Modules, RtcEntry),
          run_all_tests(TmpDir, DumpOutput, DumpError, Opts)
	; true
	),
        % even if module has no tests, we write an empty test output file
        % in order to mark that testing had been performed on a module,
        % which allows detecting newly added tests on regression testing
        write_all_test_outputs(Modules).

:- pred run_all_tests(TmpDir, DumpOutput, DumpError, RunnerArgs)
        : pathname * yesno * yesno * list(test_option).

run_all_tests(TmpDir, DumpOutput, DumpError, RunnerArgs) :-
	loader_name(BLoader),
	path_concat(TmpDir, BLoader, Loader),
	do_tests(TmpDir, Loader, DumpOutput, DumpError, RunnerArgs).

file_test_output_suffix('.testout').

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
        repeat,
        (  test_input_db(TestId, Module),
           test_output_db(TestId, TestResult)
        -> write_data(StreamOut, test_output_db(TestId, TestResult)),
           retract_fact(test_output_db(TestId, TestResult)),
           test_attributes_db(TestId, Module, F, A, Dict, Comment, S, LB, LE),
           write_data(StreamOut,
                      test_attributes_db(TestId, Module, F, A, Dict, Comment,
                              S, LB, LE)),
           retract_fact(test_attributes_db(TestId, Module, F, A, Dict,
                      Comment, S, LB, LE)),
           fail
        ;
            !
        ).

get_all_test_outputs(Modules, TestResults) :-
        get_test_outputs_(Modules, [], TestResults).

get_test_outputs_([], TestResults, TestResults).
get_test_outputs_([Module|Mods], Acc, TestResults) :-
        module_base(Module, Base),
        get_module_output(Module, Base, TestResult),
        get_test_outputs_(Mods, [ TestResult | Acc], TestResults).

get_module_output(Module, Base, TestResult) :-
        file_test_output_suffix(Suf),
        atom_concat(Base, Suf, FileModOut),
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

process_runtime_check_ta(count(ErrorStatus, Count), Module, F, A, Dict,
	    Comment, Source, LB, LE) -->
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
	    foldl(rtcheck_to_messages, RTCErrors)
	;
	    [message_lns(Source, LB, LE, note, [Module, ':', F,
			'/', A, ' (Result: ', ''({Result}), [](CountMsg),
			') Passed test', [](CommentMsg), '.', [](SignalsMsg)])]
	),
	!.


is_failed_test(st([_|_], _, _)) :- !.
is_failed_test(st(_,     _, Result)) :- is_failed_test_result(Result).

% TODO: treat PANIC in special way, other than test failure.
% TODO: similar behavior should be in rtchecks
is_failed_test_result(aborted(_, _)). % TODO: global property aborts/1 (like Major Exception), treat as test fail
is_failed_test_result(fail(precondition)).  % Show warning
is_failed_test_result(exception(precondition, _)). % PANIC
is_failed_test_result(exception(postcondition, _)). % PANIC
% TODO: is_failed_test_result(exception(predicate, _)).  assertions should assume no_exception/1 by default.

:- data test_input_db/2.
:- data test_output_db/2.
:- data test_attributes_db/9.

assert_test_input(test_input_db(A, B)) :-
	assertz_fact(test_input_db(A, B)).

assert_test_output(test_output_db(A, B)) :-
	assertz_fact(test_output_db(A, B)).

assert_test_attributes(test_attributes_db(A, B, C, D, E, F, G, H, I)) :-
	assertz_fact(test_attributes_db(A, B, C, D, E, F, G, H, I)).

dump_output(yes, StrOut) :- display_string(StrOut).
dump_output(no,  _).

dump_error(yes, StrErr) :- display_string(StrErr).
dump_error(no,  _).

:- pred do_tests(TmpDir, Loader, DumpOutput, DumpError, RunnerArgs)
        :  pathname * atm * yesno * yesno * list
# "Calls the loader as an external process. If some test aborts, calls
   recursively with the rest of the tests".
do_tests(TmpDir, Loader, DumpOutput, DumpError, RunnerArgs) :-
        do_tests_(TmpDir, Loader, DumpOutput, DumpError, RunnerArgs, no).

do_tests_(TmpDir, Loader, DumpOutput, DumpError, RunnerArgs, Resume) :-
        ( Resume = yes(ContIdx) ->
            RunnerArgs2 = [resume_after,ContIdx|RunnerArgs]
        ; RunnerArgs2 = RunnerArgs
        ),
	% this process call appends new outputs to OutFile
	process_call(Loader, RunnerArgs2,
                     [stdin(null),
                      stdout(string(StrOut)),
                      stderr(string(StrErr)),
		      status(_)]),
        %
	dump_output(DumpOutput, StrOut),
	dump_error(DumpError, StrErr),
        %
        file_test_output(BOutFile),
	path_concat(TmpDir, BOutFile, OutFile),
	retractall_fact(test_output_db(_, _)),
	assert_from_file(OutFile, assert_test_output),
        ( test_with_no_output(TestId)
        ->  % no output both in output file and output db (test aborted)
            TestResult = st([], [], aborted(StrOut, StrErr)),
            % mark the test as aborted
            open(OutFile, append, IO),
            write_data(IO, test_output_db(TestId, TestResult)),
            close(IO),
            % continue testing
            do_tests_(TmpDir, Loader, DumpOutput, DumpError, RunnerArgs, yes(TestId))
        ; true % (all tests had output)
        ).

test_with_no_output(TestId) :-
        test_input_db(TestId0,_Module),
        \+(test_output_db(TestId0,_TestResult)), !,
        TestId = TestId0.

% :- pred atom_concat_(+atm,+atm,-atm) + (not_fails, no_choicepoints).

% atom_concat_(A,B,C) :- atom_concat(A,B,C).

:- pred create_test_input(+pathname, +list(atm)) + (not_fails, no_choicepoints).

create_test_input(TmpDir, Modules) :-
	file_test_input(BFileTestInput),
	path_concat(TmpDir, BFileTestInput, FileTestInput),
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

:- pred create_global_runner(+pathname, +list, +yesno, ?atm)
	+ (not_fails, no_choicepoints).

create_global_runner(TmpDir, Modules, RtcEntry, RunnerFile) :-
        % Create wrapper files for each of the modules being tested
        create_wrapper_mods(Modules, TmpDir, RtcEntry),
        %
	runner_global_file_name(BRunnerFile),
	path_concat(TmpDir, BRunnerFile, RunnerFile),
        %
	Header = [
                    (:- module(_, [main_tests/1], [])),
		    (:- include(library(unittest/unittest_runner_base)))
		],
        % Fill the runner file with the unit test
        % instances of the form internal_runtest_module/2.
        findall(C,
                (member(Module, Modules),
                 wrapper_file_name(TmpDir, Module, WrapperFile),
                 C = (:- use_module(WrapperFile))), UseWrappers),
        findall(C,
                (member(Module, Modules),
                 module_test_entry(Module, TestEntry, TestId),
                 C = ( internal_runtest_module(Module, TestId) :- TestEntry )),
                InternalCs),
        %
        file_test_input(BFileTestInput),
        path_concat(TmpDir, BFileTestInput, FileTestInput),
        %
        Clauses = ~flatten([
            raw(Header),
            raw(UseWrappers),
            raw(InternalCs),
            raw([file_test_input_path(FileTestInput)])
        ]),
        %
	print_clauses_to_file(RunnerFile, Clauses).

create_wrapper_mods(Modules, TmpDir, RtcEntry) :-
	( % (failure-driven loop)
          member(Module, Modules),
            create_wrapper_mod(Module, TmpDir, RtcEntry),
            fail
        ; true
        ).

create_wrapper_mod(Module, TmpDir, RtcEntry) :-
        module_base(Module, Base),
        ( wrapper_file_name(TmpDir, Module, WrapperFile),
          create_module_wrapper(TmpDir, Module, RtcEntry, Base, WrapperFile)
        -> true
        ; message(error, ['Failure in create_wrapper_mod/4'])
        ).

module_test_entry(Module, TestEntry, TestId) :-
	atom_concat(Module, '$test', ModuleF),
	TestEntry =.. [ModuleF, TestId].

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
create_module_wrapper(TmpDir, Module, RtcEntry, Src, WrapperFile) :-
	ReqPackages = [assertions, nativeprops, rtchecks],
	( % TODO: Why do we need Src packages? --> maybe because the wrapper
          %   acts as a "sliced" module that contains only the test assertions
	    clause_read(Src, 1, module(_, _, SrcPackages), _, _, _, _) ->
	    union(ReqPackages, SrcPackages, Packages)
	;
	    Packages = ReqPackages
	),
	Header = [
		(:- module(_, _, Packages)),
                (:- use_module(library(unittest/unittest_runner_aux))),
                (:- use_module(library(rtchecks/rtchecks_rt))),
                (:- use_module(library(rtchecks/rtchecks_basic))),
		(:- push_prolog_flag(unused_pred_warnings, no)),
		(:- use_module(library(unittest/unittest_props))),
		(:- pop_prolog_flag(unused_pred_warnings)),
		(:- use_module(Src))
        ],
	collect_test_modules(Src, TestModules),
        % here link the TestEntry clause with the ARef test identifier
	module_test_entry(Module, TestEntry, ARef),
	findall(Clause,
                gen_test_entry(TmpDir, Module, RtcEntry, Src, TestEntry, ARef, Clause),
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

gen_test_entry(TmpDir, Module, RtcEntry, Src, TestEntry, ARef, Clause) :-
        % test entries, or default failing clause if there are none
	if(do_gen_each_test_entry(TmpDir, Module, RtcEntry, Src, TestEntry, ARef, Clause),
           true,
	   do_gen_default_test_entry(TestEntry, Clause)).

do_gen_default_test_entry(TestEntry, Clause) :-
        Clause = clause(TestEntry, fail, []).

% TODO: tests abort when the predicate is not defined in the module,
%       fix?  Depends on if we allow tests for imports -- otherwise
%       this code is still useful for writing test assertions of
%       impldef preds such as foreign.
do_gen_each_test_entry(TmpDir, Module, RtcEntry, Src, TestEntry, TestId, Clause) :-
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
        % translation of rtchecks package
        TestBody = '$test_entry_body'(TestInfo, Assertions, PLoc, TmpDir),
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
	unittest_print_clause((Head :- Body), IO, Dict).

% unittest_print_clause(Term, S, _Dict) :-
% 	current_output(CO),
% 	set_output(S),
% 	writeq(Term),
% 	write('.'),
% 	nl,
% 	set_output(CO).

unittest_print_clause(Term, S, Dict) :-
	apply_dict(Term, Dict, ATerm),
	current_output(CO),
	set_output(S),
	writeq(ATerm),
	write('.'),
	nl,
	set_output(CO).
% 	portray_clause(S, ATerm).

unittest_print_clauses(Term, S, Dict) :-
	current_output(CO),
	set_output(S),
	maplist(unittest_print_clause(S, Dict), Term),
	set_output(CO).
