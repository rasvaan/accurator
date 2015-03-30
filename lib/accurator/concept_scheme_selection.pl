:- module(concept_scheme_selection, [iconclass_code_concept_scheme/3]).

/** <module> Concept scheme selection for auto completion
*/
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle_write)).

%%	iconclass_code_concept_scheme(+Code, +ConceptScheme, +FileName)
%
%	Adds a subset of the iconclass codes to a concept scheme, so it can
%	be loaded by autocomplete
% iconclass_code_concept_scheme('http://iconclass.org/7','http://accurator.nl/bible#BiblicalThemeConceptScheme','concept_scheme_bible_theme.ttl').
iconclass_code_concept_scheme(ICCode, ConceptScheme, FileName) :-
	%find all works with class or sublcass of specified code
	find_codes(ICCode, Codes),
	save_concept_scheme(Codes, ConceptScheme, FileName).

%%	find_codes(+ICCode, -Codes)
%
%	Retrieve all codes below ICCode
find_codes(ICCode, Codes) :-
	findall(Code,
			rdf_reachable(Code, skos:broader, ICCode),
			Codes),
	length(Codes, NumberCodes),
	debug(concept_scheme, 'Number of codes of ~p or lower: ~p',
		  [ICCode, NumberCodes]).

%%	save_concept_scheme(Uris, ConceptScheme, FileName)
%
%	Assert triples linking uris to conceptschemes and save these triples
%	in a specified file.
save_concept_scheme(Uris, ConceptScheme, FileName) :-
    maplist(assert_concept_scheme(ConceptScheme), Uris),
    rdf_save_turtle(FileName, [graph(ConceptScheme)]).

assert_concept_scheme(ConceptScheme, Resource) :-
	rdf_assert(Resource, skos:inScheme, ConceptScheme, ConceptScheme),
    debug(concept_scheme, 'Asserted ~p to ~p',
	   [Resource, ConceptScheme]).
