:- module(example1,[p/1],[regrtestdecls]).

:- use_module(engine(io_basic), [nl/0]).
:- use_module(engine(io_aux), [display_list/1]).

:- regr_texec p(1.0).

p(X) :- display_list(['test_output: ',X]),nl.
