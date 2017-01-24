:- module(stats, [], [assertions, hiord]).

% A simple module for time and memory statistics

:- doc(bug, "Code around statistics/2 calls introduce some measure
   noise.").

%:- use_module(library(prolog_sys), [statistics/0]).
:- use_module(library(format), [format/3]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(between), [between/3]).

:- use_module(engine(internals), ['$empty_gcdef_bin'/0]).

% Call N times with statistics (at the end of each iteration)
:- export(stat_call/4).
:- meta_predicate stat_call(?, ?, ?, goal).
stat_call(N, I, Msg, G) :-
	format(user_error, "% Calling ~q~n", [Msg]),
	%
	( between(1, N, I),
	    stat_call_1(Msg, I, N, G),
	    fail
	; true
	).

:- meta_predicate stat_call_1(?, ?, ?, goal).
stat_call_1(Msg, I, N, G) :- 
	'$empty_gcdef_bin', % Really get rid of abolished predicates
	get_stat(Stat0),
	( call(G) -> Ok = yes ; Ok = no ),
	'$empty_gcdef_bin', % Really get rid of abolished predicates
	get_stat(Stat1),
	diff_stat(Stat0, Stat1, DiffStat),
	show_stat(Msg, I, N, DiffStat),
	%
	Ok = yes.

get_stat(s(Mem, Prg, Atms, Preds, Tw, Tr)) :-
%	statistics,
	statistics(memory, [Mem|_]),
	statistics(program, [Prg|_]),
	statistics(symbols, [Atms,Preds]),
	statistics(walltime, [Tw|_]),
	statistics(runtime, [Tr|_]).
	
diff_stat(Stat0, Stat1, DStat) :-
	Stat0 = s(Mem0, Prg0, Atms0, Preds0, Tw0, Tr0),
	Stat1 = s(Mem1, Prg1, Atms1, Preds1, Tw1, Tr1),
	DStat = s(DMem, DPrg, DAtms, DPreds, DTw, DTr),
	DMem is (Mem1 - Mem0),
	DPrg is (Prg1 - Prg0),
	DAtms is (Atms1 - Atms0),
	DPreds is (Preds1 - Preds0),
	DTw is (Tw1 - Tw0),
	DTr is (Tr1 - Tr0).

show_stat(Msg, I, N, DStat) :-
	DStat = s(DMem, DPrg, DAtms, DPreds, DTw, DTr),
	display(user_error, 'bench('),
	format(user_error, "~q", [Msg]),
	show_sep(no),
	nl(user_error),
	display(user_error, '  '),
	format(user_error, "~q", [run(I,N)]),
	display(user_error, ', statistics(['),
	nl(user_error),
	show('memory_inc', DMem, 'bytes', no),
	show('atm_and_pred_mem', DPrg, 'bytes', no),
	show('new_atoms', DAtms, 'defs', no),
	show('new_preds', DPreds, 'defs', no),
	show('walltime', DTw, 'ms', no),
	show('runtime', DTr, 'ms', yes),
	display(user_error, '  '),
	display(user_error, '])'),
	nl(user_error),
	display(user_error, ').'),
	nl(user_error).

show(Item,Value,Unit,Last) :-
	display(user_error, '  '),
	display(user_error, '  '),
	display(user_error, Item),
	display(' = '),
	display(user_error, Value),
	( Unit = '' -> true
	; display(user_error, ' /* '),
	  display(user_error, Unit),
	  display(user_error, ' */')
	),
	show_sep(Last),
	nl(user_error).

show_sep(Last) :-
	( Last = no -> display(user_error, ',') ; true ).
