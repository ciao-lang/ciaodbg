:- bundle(ciaodbg).
version('1.23.0'). % (same as 'core')
depends([core]).
alias_paths([
    library = 'lib'
]).
cmd('unittest_runner', [main='lib/unittest/unittest_runner', libexec]).
lib('lib').
manual('ciaodbg', [main='doc/SETTINGS.pl']).
