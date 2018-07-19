:- module(_, [], [lpdoclib(doccfg)]).

%! \title Config for CiaoDbg reference manual

:- include(ciao_docsrc(common/'LPDOCCOMMON')).

filepath := at_bundle(ciaodbg, 'lib').
filepath := ~ciaofilepath_common.

output_name := 'ciaodbg'.

doc_structure :=
        'ciaodbg_ref_man'-[
            'rtchecks_tutorial',
            'unittest/unittest'-[
                'unittest/unittest_props',
                'unittestdecls_doc',
                % 'unittest/unittest_utils',
                'unittest/unittest_statistics',
                'unittest/unittest_examples'],
            'profiler/profiler_doc'-[
                'profiler/profiler_utils',
                'profiler/profiler_extra',
                'profiler/profiler_auto_conf']
        ].

bibfile := ~ciao_bibfile.

allow_markdown   := yes.
syntax_highlight := yes.
