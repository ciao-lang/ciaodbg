:- module(ciaodbg_ref_man, [], [assertions]).

:- doc(filetype, application).

%:- doc(title, "Debugging, Testing and Profiling for Ciao").
:- doc(title, "Run-time Checking, Testing, and Profiling for Ciao").
:- doc(bug, "add in the main manual a reference for rtchecks documentation here.").
:- doc(bug, "move debugger here (as chapter), THEN put debugger in title.").

% :- include(core_docsrc(common/'ClipAddress')).

:- doc(summary, "This bundle implements a debugging, testing, and
profiling framework (aka
@href{https://en.wikipedia.org/wiki/Dynamic_program_analysis}{dynamic
program analysis}) for Ciao.

As other components in Ciao (like documentation generator and static
analyzer), this framework makes use of program assertions to obtain
the specification of desired behaviors (e.g., test assertions).

It is integrated into the Emacs-based development environment (under
the @tt{CiaoDbg} menu) and the Ciao builder (@tt{ciao test}
command). See reference manual for more information.  ").

