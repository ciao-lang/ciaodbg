:- bundle(ciaodbg).
version('1.18.0').
depends([core]).
alias_paths([
    library = 'lib'
]).
lib('lib').
manual('ciaodbg', [main='doc/SETTINGS.pl']).
