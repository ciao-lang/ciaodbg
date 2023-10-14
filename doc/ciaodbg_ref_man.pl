:- module(ciaodbg_ref_man, [], [assertions]).

:- doc(filetype, application).

% The (main) debugger is not here; also, 'for Ciao' is redundant. Title changed. 
%:- doc(title, "Debugging, Testing and Profiling for Ciao").
:- doc(title, "Run-time Checking, Testing, and Profiling").

:- doc(bug, "add in the main manual a reference for rtchecks documentation here.").
:- doc(bug, "move debugger here (as chapter), THEN put debugger in title.").

% :- include(core_docsrc(common/'ClipAddress')).

:- doc(summary, "This bundle contains some of the run-time checking,
testing, and profiling facilities of the Ciao program development
framework.

Similarly to other components, like the static analyzer or
the documentation generator, these parts of the framework make use of
program assertions to obtain the specification of desired behaviors
(this includes test assertions and the assertions from which run-time
tests are generated).

Some of these facilities are integrated into the Ciao builder
(@tt{ciao test} command) and the different development environments
(e.g., in Emacs --under the @tt{CiaoDbg} menu--, Playground, etc.). ").

