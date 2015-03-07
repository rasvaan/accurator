:- module(accurator, []).

/** <module> Accurator
*/

:- use_module(library(accurator/expertise)).
:- use_module(library(accurator/ui_elements)).
:- use_module(library(accurator/annotate_page)).
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
        [domain(Domain, [description('The domain'), optional(true)])]),
    Options = [domain(Domain)].

%%	get_domain_settings(-Dic, +Options)
%
%	If no domain is provided, return available domains. If domain is
%	the given option and it exists, get dict with values,
%	otherwise return dic with the generic settings
get_domain_settings(Dic, Options) :-
	option(domain(Domain), Options),
	var(Domain), !,
	findall(Domain,
			(	rdf(DomainUri, rdf:type, accu:'Domain'),
				rdf(DomainUri, rdfs:label, literal(Domain))),
			Domains),
	Dic = Domains.

get_domain_settings(Dic, Options) :-
	option(domain(Domain), Options),
	rdf(DomainUri, rdf:type, accu:'Domain'),
	rdf(DomainUri, rdfs:label, literal(Domain)),
	get_domain_dic(DomainUri, Domain, Dic).
get_domain_settings(Dic, _Options) :-
	get_domain_dic('http://accurator.nl/generic#domain', 'generic', Dic).

get_domain_dic(DomainUri, Domain, Dic) :-
	rdf(DomainUri, dcterms:requires, Taxonomy),
	rdf(DomainUri, skos:hasTopConcept, TopConcept),
	rdf(DomainUri, accu:hasMaximumExpertiseTopics, literal(MaxTopics)),
	rdf(DomainUri, accu:hasMaximumChildren, literal(MaxChildren)),
	rdf(DomainUri, accu:hasUI, UI),
	rdf(DomainUri, accu:hasDesciptiveImage, Image),
	rdf(Image, accu:hasFilePath, literal(ImagePath)),
	rdf(Image, accu:brightness, literal(Brightness)),
	Dic = domain{domain:Domain,
				 taxonomy:Taxonomy,
				 top_concept:TopConcept,
				 number_of_topics:MaxTopics,
				 number_of_children_shown:MaxChildren,
				 ui:UI,
				 image:ImagePath,
				 image_brightness:Brightness}.

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
get_annotated(Dic, Options) :-
	option(user(User), Options),
	setof(Uri, Annotation^User^
		  (	  rdf(Annotation, oa:annotatedBy, User),
			  rdf(Annotation, oa:hasTarget, Uri),
			  rdf(Uri, rdf:type, edm:'ProvidedCHO')),
		  Uris),
	!,
	Dic = artworks{uris:Uris}.

get_annotated(Dic, _Options) :-
	Dic = artworks{uris:[]}.

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
	setof(Topic, User^Expertise^
		   (   rdf(Expertise, hoonoh:from, User),
			   rdf(Expertise, hoonoh:toTopic, Topic)),
		   Topics),
	maplist(get_user_expertise(User), Topics, TopicDictPairs),
	dict_pairs(ExpertiseDict, elements, TopicDictPairs),
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

%%	get_user(+Request)
%
%	Get the id of a user.
get_user(_Request) :-
	logged_on(User),
	user_property(User, realname(RealName)),
	reply_json_dict(user{user:User, real_name:RealName}).

%%	get_user_settings(+Request)
%
%	Return saved domain and locale of user.
get_user_settings(_Request) :-
	logged_on(User),
	get_domain(User, Domain),
	get_locale(User, Locale),
	reply_json_dict(settings{locale:Locale, domain:Domain}).

get_domain(User, Domain) :-
	user_property(User, domain(Domain)), !.
get_domain(_User, "").

get_locale(User, Locale) :-
	user_property(User, locale(Locale)), !.
get_locale(_User, "").

%%	save_additional_info(+Request)
%
%	Saves the additional information about a user.
save_user_info(Request) :-
	http_read_json_dict(Request, Info0),
	atom_string(User, Info0.user),
	del_dict(user, Info0, _Value, Info),
	dict_pairs(Info, elements, InfoPairs),
	save_info_pairs(User, InfoPairs),
	reply_html_page(/,
					title('Save additional info'),
						h1('Additional info successfuly saved.')).

save_info_pairs(_User, []).
save_info_pairs(User, [Property-Value|Pairs]) :-
	InfoAtom =.. [Property, Value],
	set_user_property(User, InfoAtom),
	save_info_pairs(User, Pairs).
