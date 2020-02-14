:- bundle(ciaodbg).
version('1.18.0').
depends([core]).
alias_paths([
    library = 'lib'
]).
cmd('ciao_unittest_runner', [main='lib/unittest/unittest_runner']).
lib('lib').
manual('ciaodbg', [main='doc/SETTINGS.pl']).
