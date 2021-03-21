:- bundle(ciaodbg).
version('1.20.0').
depends([core]).
alias_paths([
    library = 'lib'
]).
cmd('unittest_runner', [main='lib/unittest/unittest_runner', libexec]).
lib('lib').
manual('ciaodbg', [main='doc/SETTINGS.pl']).
