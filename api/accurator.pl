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
:- http_handler(cliopatria(ui_text), ui_text_api,  []).

:- rdf_register_prefix(aui, 'http://semanticweb.cs.vu.nl/accurator/ui/').
:- rdf_register_prefix(abui, 'http://semanticweb.cs.vu.nl/accurator/ui/bird#').

%%	ui_text_api(+Request)
%
%	Retrieves ui text elements, according to the given locale and ui
%	screen. First it gets the url parameters, second it queries for the
%	results, after which the data is outputted
%	as json.
ui_text_api(Request) :-
    get_parameters(Request, Options),
	get_text_elements(TextDic, Options),
	reply_json_dict(TextDic).

%%	get_parameters(+Request, -Options)
%
%	Retrieves an option list of parameters from the url.
get_parameters(Request, Options) :-
    http_parameters(Request,
        [ui(UI,
			[description('The ui screen for which text elements are retrieved'),
			 optional(false)]),
		 locale(Locale,
				[description('Locale of language elements to retrieve'),
				 optional(false)])
	]),
    Options = [ui(UI), locale(Locale)].


%%	get_text_elements(-TextDic, +Options)
%
%	Retrieves text elements according to the ui and locale specified in
%	Options.
get_text_elements(TextDic, Options) :-
	option(locale(Locale), Options),
	option(ui(UI), Options),
	findall(Label-Literal,
			(	rdf(UI, Predicate, literal(lang(Locale, Literal))),
				rdf(Predicate, rdf:type, aui:uiLabel),
				iri_xml_namespace(Predicate, _, Label)),
			LabelList),
	dict_pairs(TextDic, elements, LabelList).
