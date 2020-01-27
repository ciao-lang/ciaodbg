:- bundle(ciaodbg).
version('1.18.0').
depends([core]).
alias_paths([
    library = 'lib'
]).
cmd('ciao_unittest_loader', [main='lib/unittest/unittest_loader']).
lib('lib').
manual('ciaodbg', [main='doc/SETTINGS.pl']).
