:- module(accurator_settings, []).

:- use_module(library(settings)).

/***************************************************
* Accurator settings
***************************************************/


:- setting(accurator:taxonomy, uri, 'http://purl.org/collections/nl/naturalis/ioc_birdlist_en_nl.ttl',
	   'The graph uri of the taxonomy used for supporting annotators.').
:- setting(accurator:top_concept, uri, 'http://purl.org/collections/birds/class-aves',
	   'The top concept of the taxonomy').
:- setting(accurator:number_expertise_topics, positive_integer, 50,
	   'Setting indicating the maximum number of initial topics to present to user.').
