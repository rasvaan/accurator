:- module(ui_elements, [get_elements/3]).

/** <module> Accurator UI elements
*/

:- use_module(library(semweb/rdf_db)).

%%	get_elements(+Type, -Dic, +Options)
%
%	Determine which type of UI elements to query for.
get_elements(labels, Dic, Options) :-
	get_text_elements(Dic, Options).

get_elements(countries, Dic, Options) :-
	get_countries(Dic, Options).

get_elements(languages, Dic, Options) :-
	get_languages(Dic, Options).

%%	get_countries(+DictArray, _Options)
%
%	Get a list of country names based on the geonames dataset.
get_countries(DictArray, _Options) :-
	findall(CountryDict,
			(	rdf(GeonamesCountry, gn:featureClass, gn:'A'),
				%only english for now
				rdf(GeonamesCountry, gn:name, literal(lang(en, CountryName))),
				CountryDict0 = country{},
				CountryDict1 = CountryDict0.put(name, CountryName),
				rdf(GeonamesCountry, gn:countryCode, literal(CountryCode)),
				downcase_atom(CountryCode, CountryCodeLower),
				CountryDict = CountryDict1.put(country_code, CountryCodeLower)),
			DictArray).

%%	get_languages(-DictArray, _Options)
%
%	Get a list of languages.
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
	% get all the predicates off this and possible super ui
	setof(Predicate, UI^ui_predicate(UI, Predicate), Predicates),
	maplist(get_text_element(UI, Locale), Predicates, LabelList0),
	get_selector_options(UI, Locale, SelectorFields),
	append(LabelList0, SelectorFields, LabelList),
	dict_pairs(TextDic, elements, LabelList).

ui_predicate(UI, Predicate) :-
	rdf(UI, Predicate, _Object),
	rdf(Predicate, rdf:type, auis:'UILabel').
ui_predicate(UI, Predicate) :-
	rdf(UI, rdfs:subClassOf, SuperUI),
	rdf(Predicate, rdf:type, auis:'UILabel'),
	rdf(SuperUI, Predicate, _Object).

%%	get_text_element(-Label, -Literal, +UI, +Locale)
%
%	Retrieves text elements according to the ui and locale specified in
%	Options. If the ui is not found for the specified UI, the generic
%	super class will be queried.
get_text_element(UI, Locale, Predicate, Label-Literal) :-
	rdf(UI, Predicate, literal(lang(Locale, Literal))),
	rdf(Predicate, rdf:type, auis:'UILabel'),
	!,
	iri_xml_namespace(Predicate, _, Label).

get_text_element(UI, Locale, Predicate, Label-Literal) :-
	rdf(UI, rdfs:subClassOf, SuperUI),
	rdf(SuperUI, Predicate, literal(lang(Locale, Literal))),
	rdf(Predicate, rdf:type, auis:'UILabel'),
	iri_xml_namespace(Predicate, _, Label).

%%	get_selector_options(UI, Locale, SelectorFields)
%
%	Get a list of selector fields based on the specified UI and locale.
get_selector_options(UI, Locale, SelectorFields) :-
	findall(SelectLabel-LiteralArray,
			%HACK: for now only the generic UI has selectfields.
			(	get_selector(UI, Select, SelectLabel),
				get_selector_labels(Select, Locale, LiteralArray)
			),
			SelectorFields).

get_selector(UI, Select, SelectLabel) :-
	% See if current UI has super ui (since only SuperUI has selectors).
	rdf(UI, rdfs:subClassOf, SuperUI),
	!,
	rdf(SuperUI, auis:hasSelect, Select),
	rdf(Select, rdf:type, auis:'SelectField'),
	iri_xml_namespace(Select, _, SelectLabel).

get_selector(UI, Select, SelectLabel) :-
	rdf(UI, auis:hasSelect, Select),
	rdf(Select, rdf:type, auis:'SelectField'),
	iri_xml_namespace(Select, _, SelectLabel).

get_selector_labels(Selector, Locale, LiteralDict) :-
	findall(OptionLabel-LabelDict,
			(	rdf(Selector, auis:hasSelectOption, SelectOption),
				rdf(SelectOption, rdf:type, auis:'SelectOption'),
				rdf(SelectOption, skos:prefLabel, literal(lang(Locale, Literal))),
				rdf(SelectOption, skos:notation, literal(Id)),
				iri_xml_namespace(SelectOption, _, OptionLabel),
				dict_pairs(LabelDict, elements, [label-Literal, id-Id])),
			LiteralArray),
	dict_pairs(LiteralDict, elements, LiteralArray).
