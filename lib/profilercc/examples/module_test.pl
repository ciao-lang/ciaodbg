:- module(module_test, _, []).

:- use_module(.(module3), [main3/1]).

:- use_module(library(write), [write/1]).
:- use_module(library(profilercc/profiler_auto_conf)).
:- use_module(library(profilercc/profiler_utils)).
:- use_module(library(compiler/global_module_options)).

t0 :-
    cc_auto_conf(ticks, [module1, module2, module3], main3(_A), 2, Goals,
        Tree),
    write(Goals),
    nl,
    write(Tree),
    nl.

t1 :-
    glbmod_add_package(module2, profilercc),
    glbmod_add_package(module3, profilercc),
    profile_reset,
    (\+ profile(main3(_A)) -> true ; true),
    profile_dump.

