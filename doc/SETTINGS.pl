:- module(_, [], [doccfg]).

%! \title Config for CiaoDbg reference manual

:- include(core_docsrc(docpaths)).

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
        % Profiler with cost centers
        'profilercc/profiler_doc'-[
            'profilercc/profiler_utils',
            'profilercc/profiler_extra',
            'profilercc/profiler_auto_conf'],
        % Debugging (instrumentation based)
        'tracing/traces'-[
          'byrdbox/byrd'
        ]
    ].

bibfile := ~ciao_bibfile.

allow_markdown   := yes.
syntax_highlight := yes.
