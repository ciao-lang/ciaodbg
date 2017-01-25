:- bundle(ciaodbg).
version('1.16.0-alpha.2').
depends([core]).
alias_paths([
    library = 'lib'
]).
lib('lib').
manual('ciaodbg', [main='doc/SETTINGS.pl']).
