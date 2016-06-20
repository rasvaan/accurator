:- module(conf_accurator, []).

/** <module> Accurator annotation system.
*/

:- use_module(library('semweb/rdf_library')).

% Load ui rdf
:- rdf_attach_library(accurator(rdf)).
:- rdf_load_library('accurator-generic').
:- rdf_load_library('accurator-example-object').
%:- rdf_load_library('accurator-bird-domain').
%:- rdf_load_library('accurator-bible-domain').
%:- rdf_load_library('accurator-fashion-domain').

:- use_module(api(accurator)).
