# Debugging, Testing and Profiling for Ciao

This bundle implements a debugging, testing and profiling framework
(aka
[dynamic program analysis](https://en.wikipedia.org/wiki/Dynamic_program_analysis)
for Ciao.

As other components in Ciao (like documentation generator and static
analyzer), this framework makes use of program assertions to obtain
the specification of desired behaviors (e.g., test assertions).

It is integrated into the Emacs-based development environment (under
the `CiaoDbg` menu) and the Ciao builder (`ciao test` command). See
reference manual for more information.
