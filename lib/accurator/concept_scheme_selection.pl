:- module(concept_scheme_selection, [iconclass_code_concept_scheme/3,
									 type_concept_scheme/3]).

/** <module> Concept scheme selection for auto completion
*/
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle_write)).

:- rdf_register_prefix(bibleontology, 'http://bibleontology.com/resource/').


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

%%	type_concept_scheme(+Type, +ConceptScheme, +FileName)
%
%	Adds the specified type to a concept scheme.
%type_concept_scheme('http://bibleontology.com/class/Biblical_Figures', 'http://accurator.nl/bible#BiblicalFigureConceptScheme', 'concept_scheme_bible_figure.ttl').
type_concept_scheme(Type, ConceptScheme, FileName) :-
	%find all works with class or sublcass of specified code
	find_type(Type, Uris),
	save_concept_scheme(Uris, ConceptScheme, FileName).

%%	find_type(+Type, -Uris)
%
%	Retrieve all uris of type Type
find_type(Type, Uris) :-
	findall(Uri,
			rdf(Uri, rdf:type, Type),
			Uris),
	length(Uris, NumberUris),
	debug(concept_scheme, 'Number of uris of type ~p: ~p',
		  [Type, NumberUris]).

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
