:- module(accurator_settings, []).

:- use_module(library(settings)).

/***************************************************
* Accurator settings
***************************************************/


:- setting(accurator:taxonomy, uri, 'http://purl.org/collections/nl/naturalis/ioc_birdlist_en.ttl',
	   'The graph uri of the taxonomy used for supporting annotators.').
:- setting(accurator:image_filter, atom, 'http://purl.org/collections/birds/class-aves',
	   'The top concept of the taxonomy').
:- setting(accurator:number_expertise_topics, positive_integer, 50,
	   'Setting indicating the level of enrichment of results.').
