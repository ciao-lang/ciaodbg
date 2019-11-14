:- module(regrtest_aux,[clean_output/3],[dcg]).

:- use_module(engine(internals), [ciao_root/1]).

% clean_output(Out,In,Unparsed)
clean_output(Cs) -->
    { ciao_root(CiaoRoot), atom_codes(CiaoRoot,RootPath) }, % TODO: does not work if testing bundles not in ciao_root!
    clean_output_(Cs, RootPath).

% remove absolute file paths
clean_output_("\n\n{In " || Cs    , RootPath) --> "{In ", !,
    clean_filename(Cs,Cs0,RootPath),
    clean_output_(Cs0,RootPath).
clean_output_("'" || Cs, RootPath) --> "loc('",!,
    clean_filename(Cs,Cs0,RootPath),
    clean_output_(Cs0,RootPath).
% clean ciao version
clean_output_(Cs                  , RootPath) --> "Ciao ", !,
    clean_ciaovers(Cs,Cs0),
    clean_output_(Cs0, RootPath).
% clean source expansions (*_co.pl)
clean_output_(Cs          , RootPath) --> "{Note: ",!,
    clean_expander(Cs, Cs0),
    clean_output_(Cs0,RootPath).
%
clean_output_("\n" || Cs          , RootPath) --> "\n\n\n",!,
    clean_output_(Cs,RootPath).
clean_output_([C|Cs]              , RootPath) --> [C], !,
    clean_output_(Cs, RootPath).
clean_output_([]                  ,_RootPath) --> [].

clean_filename(Cs, Cs0, RootPath) -->
    ( cs(RootPath) ->
        { Cs = "$CIAOROOT" || FileName }
    ; { Cs = FileName }
    ),
    clean_filename_(FileName, Cs0).

clean_filename_(".pl" || Cs, Cs)  --> ".pl", !.
clean_filename_([C|Cs]     , Cs0) --> [C],
    clean_filename_(Cs,Cs0).

cs([C|Cs]) --> [C], cs(Cs).
cs([]    ) --> [].

clean_ciaovers(Cs,Cs)  --> "]", !.
clean_ciaovers(Cs,Cs0) --> [_], clean_ciaovers(Cs,Cs0).

clean_expander(Cs,Cs)  --> "}", !.
clean_expander(Cs,Cs0) --> [_], clean_expander(Cs,Cs0).

