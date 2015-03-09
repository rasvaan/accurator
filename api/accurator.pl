:- module(accurator, []).

/** <module> Accurator
*/

:- use_module(library(accurator/accurator_user)).
:- use_module(library(accurator/annotate_page)).
:- use_module(library(accurator/domain)).
:- use_module(library(accurator/expertise)).
:- use_module(library(accurator/recommendation/strategy_random)).
:- use_module(library(accurator/recommendation/strategy_expertise)).
:- use_module(library(accurator/ui_elements)).
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
:- http_handler(cliopatria('annotate_image.html'), http_image_annotation, []).
:- http_handler(cliopatria(ui_elements), ui_elements_api,  []).
:- http_handler(cliopatria(domains), domains_api,  []).
:- http_handler(cliopatria(recently_annotated), recently_annotated_api,  []).
:- http_handler(cliopatria(expertise_topics), expertise_topics_api,  []).
:- http_handler(cliopatria(expertise_values), expertise_values_api,  []).
:- http_handler(cliopatria(register_user), register_user,  []).
:- http_handler(cliopatria(get_user), get_user,  []).
:- http_handler(cliopatria(get_user_settings), get_user_settings,  []).
:- http_handler(cliopatria(save_user_info), save_user_info,  []).
:- http_handler(cliopatria(recommendation), recommendation_api, []).

:- rdf_register_prefix(auis, 'http://accurator.nl/ui/schema#').
:- rdf_register_prefix(aui, 'http://accurator.nl/ui/generic#').
:- rdf_register_prefix(ausr, 'http://accurator.nl/user#').
:- rdf_register_prefix(as, 'http://accurator.nl/schema#').
:- rdf_register_prefix(edm, 'http://www.europeana.eu/schemas/edm/').
:- rdf_register_prefix(gn, 'http://www.geonames.org/ontology#').
:- rdf_register_prefix(txn, 'http://lod.taxonconcept.org/ontology/txn.owl#').
:- rdf_register_prefix(oa, 'http://www.w3.org/ns/oa#').
:- rdf_register_prefix(hoonoh, 'http://hoonoh.com/ontology#').

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

%%	get_parameters_elements(+Request, -Options)
%
%	Retrieves an option list of parameters from the url.
get_parameters_elements(Request, Options) :-
    http_parameters(Request,
        [ui(UI,
		    [description('UI for which text elements are retrieved'),
			 optional(false)]),
		 locale(Locale,
			[description('Locale of language elements to retrieve'),
			 optional(false)]),
		 type(Type,
			[description('Type of elements to retrieve'),
			 optional(type)])
	]),
    Options = [ui(UI), locale(Locale), type(Type)].

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

%%	domain_settings_api(+Request)
%
%	Retrieves the settings specific to a domain.
domains_api(Request) :-
    get_parameters_domain(Request, Options),
	get_domain_settings(Dic, Options),
	reply_json_dict(Dic).

%%	get_parameters_domain(+Request, -Options)
%
%	Retrieves an option list of parameters from the url.
get_parameters_domain(Request, Options) :-
    http_parameters(Request,
        [domain(Domain,
		    [description('The domain'),
			 optional(true)])]),
    Options = [domain(Domain)].

%%	expertise_topics_api(+Request)
%
%	Retrieves a list of expertise topics.
expertise_topics_api(Request) :-
    get_parameters_expertise(Request, Options),
	get_expertise_topics(Dic, Options),
	reply_json_dict(Dic).

%%	get_parameters_expertise(+Request, -Options)
%
%	Retrieves an option list of parameters from the url.
get_parameters_expertise(Request, Options) :-
    http_parameters(Request,
        [locale(Locale,
		    [description('Locale of language elements to retrieve'),
			 optional(false)]),
		taxonomy(Taxonomy,
			[description('Domain specific taxonomy.'),
			 optional(false)]),
		number_of_topics(NumberOfTopicsString,
			[description('The maximum number of topics to retrive'),
			 optional(false)]),
		top_concept(TopConcept,
			[description('The top concept to start searching form.'),
			 optional(false)]),
		number_of_children_shown(NumberOfChildrenString,
			[description('The number of child concepts shown.'),
			 optional(true),
			 default(3)])]),
    atom_number(NumberOfTopicsString, NumberOfTopics),
	atom_number(NumberOfChildrenString, NumberOfChildren),
	Options = [locale(Locale), taxonomy(Taxonomy),
			   topConcept(TopConcept), numberOfTopics(NumberOfTopics),
			   numberOfChildren(NumberOfChildren)].

%%  expertise_values_api(+Request)
%
%	Depending on the request either save or retrieve the user expertise
%	values.
expertise_values_api(Request) :-
	member(method(get), Request),
	logged_on(User),
	get_domain(User, Domain),
	get_user_expertise_domain(User, Domain, ExpertiseValues),
	dict_pairs(ExpertiseDict, elements, ExpertiseValues),
	reply_json_dict(ExpertiseDict).

expertise_values_api(Request) :-
	member(method(post), Request),
	http_read_json_dict(Request, JsonIn), !,
	atom_string(User, JsonIn.user),
	Expertise = JsonIn.expertise,
	dict_pairs(Expertise, elements, ExpertisePairs),
	maplist(assert_expertise_relationship(User), ExpertisePairs),
	reply_html_page(/,
					title('Assert expertise values'),
						h1('Successfully asserted expertise values')).

%%  recommendation_api(+Request)
%
%
recommendation_api(Request) :-
    get_recommendation_parameters(Request, Options),
    option(strategy(Strategy), Options),
    strategy(Strategy, Result, Options),
    reply_json(Result).

%%	get_parameters(+Request, -Parameters)
%
%   Retrieves an option object of parameters from the url.
get_recommendation_parameters(Request, Options) :-
	logged_on(User),
    http_parameters(Request,
        [strategy(Strategy,
		    [default(random),
			 oneof([random, expertise])]),
		 target(Target,
			[default('http://www.europeana.eu/schemas/edm/ProvidedCHO')])
		]),
    Options = [strategy(Strategy), user(User),
			   target(Target)].

%%	strategy(+Strategy, -Result, +Options)
%
%   Selects objects according to the specified strategy.
strategy(random, Result, Options) :-
    strategy_random(Result, Options).

strategy(expertise, Result, Options) :-
    strategy_expertise(Result, Options).

