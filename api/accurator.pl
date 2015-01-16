:- module(accurator, []).

/** <module> Accurator
*/

:- use_module(library(accurator/settings)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(user(user_db)).

http:location(html, cliopatria(html), []).
http:location(img, cliopatria(img), []).
user:file_search_path(html, web(html)).
user:file_search_path(img, web(img)).

:- http_handler(cliopatria('.'), serve_files_in_directory(html), [prefix]).
:- http_handler(img('.'), serve_files_in_directory(img), [prefix]).
:- http_handler(cliopatria(ui_elements), ui_elements_api,  []).
:- http_handler(cliopatria(recently_annotated), recently_annotated_api,  []).
:- http_handler(cliopatria(expertise_topics), expertise_topics_api,  []).

:- http_handler(cliopatria(register_user), register_user,  []).

:- rdf_register_prefix(aui, 'http://semanticweb.cs.vu.nl/accurator/ui/').
:- rdf_register_prefix(abui, 'http://semanticweb.cs.vu.nl/accurator/ui/bird#').
:- rdf_register_prefix(gn, 'http://www.geonames.org/ontology#').
:- rdf_register_prefix(txn, 'http://lod.taxonconcept.org/ontology/txn.owl#').


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

%%	recently_annotated_api(+Request)
%
%	Retrieves a list of artworks the user recently annotated.
recently_annotated_api(Request) :-
    get_parameters_annotated(Request, Options),
	get_annotated(Dic, Options),
	reply_json_dict(Dic).

%%	get_parameters_annotated(+Request, -Options)
%
%	Retrieves an option list of parameters from the url.
get_parameters_annotated(Request, Options) :-
    http_parameters(Request,
        [user(User, [description('The user'), optional(false)])]),
    Options = [user(User)].

%%	get_annotated(-Dic, +Options)
%
%	Query for a list of artworks the user recently annotated.
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

%%	expertise_topics_api(+Request)
%
%	Retrieves a list of expertise topics.
expertise_topics_api(Request) :-
    get_parameters_expertise(Request, Options),
	get_expertise_topics(Dic, Options),
	reply_json_dict(Dic).

%%	expertise_topics(+Request)
%
%	Retrieves a list of expertise topics.
get_expertise_topics(Topics, Options) :-
	setting(accurator:top_concept, TopConcept),
	setting(accurator:number_expertise_topics, Number),
	option(locale(Locale), Options),
	get_number_topics([TopConcept], Number, TopicUris),
	maplist(get_info_topics(Locale), TopicUris, TopicDicts),
	Topics = expertise_topics{topics:TopicDicts}.

get_info_topics(Locale, Uri, Dict) :-
	rdf_global_id(Uri, GlobalUri),
	get_label(Locale, Uri, Label),
	get_childrens_labels(Uri, Locale, 3, ChildrensLabels),
	Dict = topic{uri:GlobalUri, label:Label, childrens_labels:ChildrensLabels}.

get_label(Locale, Uri, Label) :-
	rdf(Uri, txn:commonName, literal(lang(Locale, Label))), !.

get_label(Locale, Uri, Label) :-
	rdf(Uri, skos:prefLabel, literal(lang(Locale, Label))), !.

get_label(Locale, Uri, Label) :-
	rdf_has(Uri, skos:prefLabel, literal(lang(Locale, Label))), !.

%ioc specific, to get common names when possible
get_label(_Locale, Uri, Label) :-
	rdf(Uri, txn:commonName, literal(Label)), !.

get_label(_Locale, Uri, Label) :-
	rdf(Uri, skos:prefLabel, literal(Label)), !.

get_label(_Locale, Uri, Label) :-
	rdf_has(Uri, skos:prefLabel, literal(Label)), !.

get_childrens_labels(Uri, Locale, MaxNumber, Labels) :-
	get_children(Uri, Children),
	maplist(get_label(Locale), Children, LongLabels),
	shorten_when_needed(LongLabels, MaxNumber, Labels).

shorten_when_needed(LongLabels, MaxNumber, LongLabels) :-
	length(LongLabels, Length),
	Length =< MaxNumber, !.

shorten_when_needed(LongLabels, MaxNumber, Labels) :-
	append(Labels, _Rest, LongLabels),
	length(Labels, MaxNumber).

%%	get_number_topics(Concepts, Number, PreviousTopics, PreviousTopics)
%
%	Find a list of topics, smaller than the specified number. If the new
%	number exceeds the specified number, the previous list of concepts
%	is returned.
get_number_topics(Concepts, Number, Topics) :-
	maplist(get_children, Concepts, ChildrenLists),
	append(ChildrenLists, Children),
	length(Children, NumberChildren),
	NumberChildren < Number,
	get_number_topics(Children, Number, Topics).
get_number_topics(Topics, _Number, Topics) :-
	!.

%%	get_children(+Concept, -ChildrenList)
%
%	Get a list of children for the concept.
get_children(Concept, ChildrenList) :-
	findall(Child,
			rdf_has(Child, skos:broader, Concept),
			ChildrenList).

%%	get_parameters_expertise(+Request, -Options)
%
%	Retrieves an option list of parameters from the url.
get_parameters_expertise(Request, Options) :-
    http_parameters(Request,
        [locale(Locale, [description('Locale of language elements to retrieve'), optional(false)])]),
    Options = [locale(Locale)].

%%	register_user(+Request)
%
%	Register a user using information within a json object.
register_user(Request) :-
	http_read_json_dict(Request, JsonIn),
	atom_string(User, JsonIn.user),
	(   current_user(User)
	->  throw(error(permission_error(create, user, User),
			context(_, 'User already exists')))
	;   atom_string(JsonIn.password, Password),
		password_hash(Password, Hash),
		atom_string(RealName, JsonIn.name),
			Allow = [ read(_,_), write(_,annotate) ],
		user_add(User, [realname(RealName), password(Hash), allow(Allow)]),
		reply_html_page(/,
					title('Register user'),
						h1('Successfully registered user'))
	).
