:- module(_,_,[debugpred]).

:- use_module(engine(io_basic), [display/1]).

:- debugpredstatus(off).

:- debugpred debug_display/1.

main :-
	debug_display('This is a debugging message.'),
	display('Normal message\n').

debug_display(A) :- display(A).
