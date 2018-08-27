:- module(graphic_trace, [], [assertions]).

% Warning: File encoded in UTF-8.
   
:- doc(title, "Predicate Tracing Facilities").
:- doc(author, "Jose F. Morales").

:- doc(bug, "Merge with static_trace library, document").
:- doc(bug, "Use locale (LANG env var) to check if the tty can display unicode").
:- doc(bug, "Allow different print levels (grouped, event-based, human
   readable, machine readable, etc.)").

:- use_module(engine(messages_basic), [message/2]).
:- use_module(library(lists), [append/3]).

% trace_level(Level, Mod)
:- data trace_level/2.
% trace_status(Mod, Status)
:- data trace_status/2.

:- export(reset_trace/1).
% Reset tracing for module Mod
reset_trace(Mod) :-
	retractall_fact(trace_status(Mod, _)),
	retractall_fact(trace_level(_, Mod)).

:- export(set_trace/2).
% Set tracing status for module Mod
set_trace(Mod, Status) :-
	retractall_fact(trace_status(Mod, _)),
	assertz_fact(trace_status(Mod, Status)).

% Get trace level for Mod
get_trace_level(Mod, L) :-
	( trace_level(L, Mod) -> true ; L = 0 ).

% Set trace level for Mod
set_trace_level(Mod, L) :-
	retractall_fact(trace_level(_, Mod)),
	assertz_fact(trace_level(L, Mod)).

% Increment/decrement trace level for Mod
inc_trace_level(Mod, I) :-
	get_trace_level(Mod, L),
	L1 is L + I,
	set_trace_level(Mod, L1).

:- export(trace_begin/2).
% Begin a trace block for Mod
trace_begin(Mod, _Msg) :-
	trace_status(Mod, off),
	!.
trace_begin(Mod, Msg) :-
	trace_msg(Msg, Msg2),
	Msg3 = ['┌─'|Msg2],
%	Msg3 = ['╭─'|Msg2],
	print_msg(Mod, Msg3),
	inc_trace_level(Mod, 1).

:- export(trace_end/2).
% End a trace block for Mod
trace_end(Mod, _Msg) :-
	trace_status(Mod, off),
	!.
trace_end(Mod, Msg) :-
	inc_trace_level(Mod, -1),
	trace_msg(Msg, Msg2),
	append(['└─'|Msg2], [' {exit}'], Msg3),
%	append(['╰─'|Msg2], [' {exit}'], Msg3),
	print_msg(Mod, Msg3).

:- export(trace/2).
% Show a simple trace line
trace(Mod, _Msg) :-
	trace_status(Mod, off),
	!.
trace(Mod, Msg) :-
	trace_msg(Msg, Msg2),
	print_msg(Mod, Msg2).

% Print indented message
print_msg(Mod, Msg) :-
	indent_msg(Mod, Msg, Msg2),
	message(user, Msg2).

% Add indentation marks after each '\n'
indent_msg(Mod, Xs, Ys) :-
	get_trace_level(Mod, I),
	indent_nl(I, Ys, Ys2), 
	indent_msg_(Xs, I, Ys2).

indent_msg_([], _I, []) :- !.
indent_msg_([X|Xs], I, [X|Ys]) :-
	X = '\n', !,
	indent_nl(I, Ys, Ys2), 
	indent_msg_(Xs, I, Ys2).
indent_msg_([X|Xs], I, [X|Ys]) :-
	indent_msg_(Xs, I, Ys).

indent_nl(I, Ys, Ys0) :- I =< 0, !, Ys = Ys0.
indent_nl(I, ['│ '|Ys], Ys0) :-
	I1 is I - 1,
	indent_nl(I1, Ys, Ys0).

:- multifile m_trace_msg/2.

trace_msg(X, X) :- X = [_|_], !.
trace_msg(X, Msg) :-
	m_trace_msg(X, Msg).




