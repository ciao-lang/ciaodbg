:- module(_, [], [doccfg]).

%! \title Config for CiaoDbg reference manual

:- include(ciao_docsrc(common/'LPDOCCOMMON')).

filepath := at_bundle(ciaodbg, 'lib').
filepath := ~ciaofilepath_common.

output_name := 'ciaodbg'.

doc_structure :=
        'ciaodbg_ref_man'-[
	    % Runtime-checks
            'rtchecks_tutorial',
            % Unit tests
            'unittest/unittest'-[
                'unittest/unittest_props',
                'unittestdecls_doc',
                % 'unittest/unittest_utils',
                'unittest/unittest_statistics',
                'unittest/unittest_examples'],
            % Profiling
            'profiler/profiler_doc'-[
                'profiler/profiler_utils',
                'profiler/profiler_extra',
                'profiler/profiler_auto_conf'],
	    % Debugging (instrumentation based)
	    'tracing/traces'-[
              'byrdbox/byrd'
            ]
        ].

bibfile := ~ciao_bibfile.

allow_markdown   := yes.
syntax_highlight := yes.
