:- module(accurator, []).

/** <module> Accurator
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).

http:location(html, cliopatria(html), []).
http:location(img, cliopatria(img), []).
user:file_search_path(html, web(html)).
user:file_search_path(img, web(img)).

:- http_handler(cliopatria('.'), serve_files_in_directory(html), [prefix]).
:- http_handler(img('.'), serve_files_in_directory(img), [prefix]).
:- http_handler(cliopatria(ui_elements), ui_elements_api,  []).
:- http_handler(cliopatria(recently_annotated), recently_annotated_api,  []).

:- rdf_register_prefix(aui, 'http://semanticweb.cs.vu.nl/accurator/ui/').
:- rdf_register_prefix(abui, 'http://semanticweb.cs.vu.nl/accurator/ui/bird#').
:- rdf_register_prefix(gn, 'http://www.geonames.org/ontology#').

%%	recently_annotated_api(+Request)
%
%	Retrieves a list of artworks the user recently annotated.
recently_annotated_api(Request) :-
    get_parameters_annotated(Request, Options),
	get_annotated(Dic, Options),
	reply_json_dict(Dic).

get_annotated(Dic, _Options) :-
	%option(user(User), Options),
	Dic = artworks{uris:['http://purl.org/collections/nl/naturalis/print-134677',
				  'http://purl.org/collections/nl/naturalis/print-134685',
				  'http://purl.org/collections/nl/naturalis/print-134699',
				  'http://purl.org/collections/nl/naturalis/print-134703',
				  'http://purl.org/collections/nl/naturalis/print-134710',
				  'http://purl.org/collections/nl/naturalis/print-134717',
				  'http://purl.org/collections/nl/naturalis/print-134731',
				  'http://purl.org/collections/nl/naturalis/print-134766',
				  'http://purl.org/collections/nl/naturalis/print-134767']}.

%%	get_parameters_annotated(+Request, -Options)
%
%	Retrieves an option list of parameters from the url.
get_parameters_annotated(Request, Options) :-
    http_parameters(Request,
        [user(User, [description('The user'), optional(false)])]),
    Options = [user(User)].

%%	ui_elements_api(+Request)
%
%	Retrieves ui elements, according to the given locale, type and ui
%	screen. First it gets the url parameters, second it queries for the
%	results, after which the data is outputted
%	as json.
ui_elements_api(Request) :-
    get_parameters_elements(Request, Options),
	option(type(Type), Options),
	get_elements(Type, Dic, Options),
	reply_json_dict(Dic).

%%	get_parameters(+Request, -Options)
%
%	Retrieves an option list of parameters from the url.
get_parameters_elements(Request, Options) :-
    http_parameters(Request,
        [ui(UI,
			[description('The ui screen for which text elements are retrieved'),
			 optional(false)]),
		 locale(Locale,
				[description('Locale of language elements to retrieve'),
				 optional(false)]),
		 type(Type,
				[description('Type of elements to retrieve'),
				 optional(type)])
	]),
    Options = [ui(UI), locale(Locale), type(Type)].

get_elements(labels, Dic, Options) :-
	get_text_elements(Dic, Options).

get_elements(countries, Dic, Options) :-
	get_countries(Dic, Options).

get_elements(languages, Dic, Options) :-
	get_languages(Dic, Options).


get_countries(DictArray, _Options) :-
	findall(CountryDict,
			(	rdf(GeonamesCountry, gn:featureClass, gn:'A'),
				%only english for now
				rdf(GeonamesCountry, gn:name, literal(lang(en, CountryName))),
				CountryDict0 = country{},
				CountryDict1 = CountryDict0.put(name, CountryName),
				rdf(GeonamesCountry, gn:geonamesID, literal(GeonamesIDInt)),
				CountryDict = CountryDict1.put(geo_id, GeonamesIDInt)),
			DictArray).

get_languages(DictArray, _Options) :-
	findall(LanguageDict,
			(	rdf(GeonamesCountry, acl:isoCode, literal(IsoCode)),
				LanguageDict0 = language{},
				LanguageDict1 = LanguageDict0.put(iso_code, IsoCode),
				%only english for now
				rdf(GeonamesCountry, rdfs:label, literal(lang(en, LanguageName))),
				LanguageDict = LanguageDict1.put(name, LanguageName)),
			DictArray).

%%	get_text_elements(-TextDic, +Options)
%
%	Retrieves text elements according to the ui and locale specified in
%	Options.
get_text_elements(TextDic, Options) :-
	option(locale(Locale), Options),
	option(ui(UI), Options),
	findall(Label-Literal,
			(	rdf(UI, Predicate, literal(lang(Locale, Literal))),
				rdf(Predicate, rdf:type, aui:'UILabel'),
				iri_xml_namespace(Predicate, _, Label)),
			LabelList0),
	get_selector_options(UI, Locale, SelectorFields),
	append(LabelList0, SelectorFields, LabelList),
	dict_pairs(TextDic, elements, LabelList).

get_selector_options(UI, Locale, SelectorFields) :-
	findall(SelectorLabel-LiteralArray,
			(	rdf(UI, aui:hasSelector, Selector),
				rdf(Selector, rdf:type, aui:'SelectorField'),
				 iri_xml_namespace(Selector, _, SelectorLabel),
				get_selector_labels(Selector, Locale, LiteralArray)
				),
			SelectorFields).

get_selector_labels(Selector, Locale, LiteralDict) :-
	findall(OptionLabel-Literal,
			(	rdf(Selector, Predicate, literal(lang(Locale, Literal))),
				rdf(Predicate, rdf:type, aui:'UILabel'),
				iri_xml_namespace(Predicate, _, OptionLabel)),
			LiteralArray),
	dict_pairs(LiteralDict, elements, LiteralArray).
