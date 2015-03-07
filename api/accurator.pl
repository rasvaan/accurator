:- module(accurator, []).

/** <module> Accurator
*/

:- use_module(library(accurator/expertise)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(user(user_db)).
:- use_module(applications(annotation)).
:- use_module(user(preferences)).

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

%%	get_elements(+Type, -Dic, +Options)
%
%	Determine which type of UI elements to query for.
get_elements(labels, Dic, Options) :-
	get_text_elements(Dic, Options).

get_elements(countries, Dic, Options) :-
	get_countries(Dic, Options).

get_elements(languages, Dic, Options) :-
	get_languages(Dic, Options).

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
	set_preferred_language(Property, Value),
	InfoAtom =.. [Property, Value],
	set_user_property(User, InfoAtom),
	save_info_pairs(User, Pairs).

%%	set_preferred_language(+Setting, +Locale)
%
%	Save the preferred locale, so it can be used for the annotation
%  interface fields.
set_preferred_language(locale, Lang) :-
	user_preference(user:lang, literal(Lang)).
set_preferred_language(_, _).

http_image_annotation(Request) :-
	get_annotation_parameters(Request, Options),
	reply_page(Options).

get_annotation_parameters(Request, Options) :-
	http_parameters(Request,
	[ uri(Uri,
			 [uri,
			  description('URI of the object to be annotated') ])
	]),
	Options = [uri(Uri)].

reply_page(_Options) :-
	% Sent to intro page when not logged in
	not(logged_on(_User)),
	reply_html_page(
		[title('Not logged in'),
		 meta([name('viewport'),content('width=device-width, initial-scale=1.0')])
		],
		[script(type('text/javascript'), 'document.location.href="intro.html"')]).

reply_page(Options) :-
    option(uri(Uri), Options),
	logged_on(User),
	get_annotation_ui(User, UI),
	get_anfields(UI, [], [], AnnotationFields),
	debug(anno, 'UI: ~p', [UI]),
	AnnotationOptions = [targets([Uri]),
						 ui(UI),
						 annotation_fields(AnnotationFields),
						 metadata_fields(['http://semanticweb.cs.vu.nl/annotate/ui/imageURL']),
						 user(User)],
	http_absolute_location(cliopatria('img/favicon.ico'), LogoUrl, []),
	reply_html_page(
	[title(Uri),
	 link([href(LogoUrl),rel('shortcut icon')]),
	 meta([name('viewport'),content('width=device-width, initial-scale=1.0')])
    ],
	[\navigation_bar,
	  div([ class('container')],
		  [\annotation_page_body(AnnotationOptions),
		   \navigation,
		   \metadata,
		   \login_modal,
		   \annotate_javascript,
		   \html_requires(css('bootstrap.min.css')),
		   \html_requires(css('accurator.css'))])]).

get_annotation_ui(User, UI) :-
	user_property(User, domain(Domain)),
	atom_string(DomainAtom, Domain),
	atomic_list_concat(['http://accurator.nl/', DomainAtom, '#domain'], DomainUri),
	rdf(DomainUri, accu:hasAnnotationUI, UI).

%%	navigation_bar(+Page)
%
%	Generates the navigation bar.
navigation_bar -->
    html({|html||
	<!-- Navbar -->
	<nav class="navbar navbar-inverse navbar-fixed-top" role="navigation">
		 <div class="container-fluid">
			<div class="navbar-header">
				<button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#accuratorCollapse">
					<span class="icon-bar"></span>
					<span class="icon-bar"></span>
				</button>
				<a class="navbar-brand" href="intro.html">
					<img id="headerImage" src="img/accurator.png" alt="Accurator">
				</a>
			</div>
			<div class="collapse navbar-collapse" id="accuratorCollapse">
				<ul class="nav navbar-nav navbar-right userDropdown">
				</ul>
				<div class="navbar-form navbar-nav" id="frmGroupSearch">
					<div class="form-group">
						<input type="text" class="form-control" id="frmSearch">
					</div>
					<button id="btnAnnotateSearch" class="btn btn-default">
					</button>
				</div>
				<button class="btn navbar-btn navbar-right btn-primary" id="btnAnnotateRecommend">
				</button>
			</div>
		</div>
	</nav>
	|}).

annotate_javascript -->
    html({|html||
	<!-- Added Script -->
	<script type="text/javascript" src="js/accurator.jquery.min.js"></script>
	<script type="text/javascript" src="js/accurator.bootstrap.min.js"></script>
	<script type="text/javascript" src="js/accurator.laconic.js"></script>
	<script type="text/javascript" src="js/pengines.js"></script>
	<script type="text/javascript" src="js/result.js"></script>
	<script type="text/javascript" src="js/accurator_utilities.js"></script>
	<script type="text/javascript" src="js/accurator_annotate.js"></script>
	<script>annotateInit()</script>
	|}).
navigation -->
	html({|html||
	<!-- Navigation -->
	<div class="row" id="navigation">
	    <div class="col-md-2">
			<button class="btn btn-default navButton" id="btnPrevious">
		        <span class="glyphicon glyphicon-chevron-left"></span>
			</button>
		</div>
		<div class="col-md-8">
			<div class="form-inline" id="frmSearchAnnotate">
		        <button class="btn btn-primary navButton" id="btnResultRecommend">
				</button>
				<div class="form-group">
				    <input class="form-control" id="frmSearch" type="text">
				</div>
				<button id="btnResultSearch" class="btn btn-default navButton">
				</button>
			</div>
		</div>
		<div class="col-md-2" id="btnAlignRight">
			<button class="btn btn-default navButton" id="btnNext">
		        <span class="glyphicon glyphicon-chevron-right"></span>
			</button>
		</div>
	</div>
	|}).
metadata -->
	html({|html||
	<!-- Metadata -->
	<div id="metadata"></div>
	|}).
login_modal -->
    html({|html||
	<!-- Login modal -->
	<div class="modal fade" id="modalLogin">
		<div class="modal-dialog">
			<div class="modal-content">
				<div class="modal-header">
					<button type="button" class="close" id="mdlBtnClose">&times;</button>
					<h4 id="mdlTxtTitle">
					</h4>
				</div>
				<div class="modal-body">
					<form role="form">
						<div class="form-group">
							<label id="mdlFrmUsername" for="inputUsername">
							</label>
							<input type="text" class="form-control" id="inputUsername">
						</div>
						<div class="form-group">
							<label id="mdlFrmPassword" for="password">
							</label>
							<input type="password" class="form-control" id="inputPassword">
						</div>
						<p class="text-warning" id="mdlLblLogin">
						</p>
					</form>
				</div>
				<div class="modal-footer">
					<button class="btn btn-primary" id="mdlBtnLogin">
					</button>
				</div>
			</div>
		</div>
	</div>
	|}).
