:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for ciaodbg").

% ===========================================================================
:- doc(section, "Configuration rules").

:- use_module(library(process), [process_call/3]).
:- use_module(library(system), [find_executable/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).

:- discontiguous(m_bundle_foreign_config_tool/3).

% ===========================================================================
% (nested)

'$builder_hook'(item_nested(profiler)).
:- include(.('profiler.hooks')).
