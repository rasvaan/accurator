:- module(conf_accurator, []).

/** <module> Accurator annotation system.
*/

:- use_module(library('semweb/rdf_library')).

% Load ui rdf
:- rdf_attach_library(accurator(rdf)).
:- rdf_load_library('accurator-bird-ui').

:- use_module(api(accurator)).