:- module(_, [main/1], [hiord]).

:- use_module(library(compiler), [use_module/1]).

main([RunnerFile|Args]) :- use_module(RunnerFile), _:main_tests(Args).
