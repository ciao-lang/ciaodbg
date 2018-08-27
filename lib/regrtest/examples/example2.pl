:- module(example2,[p/1],[regrtestdecls,assertions,rtchecks]).

:- use_module(engine(io_basic), [nl/0]).
:- use_module(engine(messages_basic), [display_list/1]).

:- regr_texec p(1.0).

:- pred p(N) : int(N).

p(X) :- display_list(['test_output: ',X]),nl.
