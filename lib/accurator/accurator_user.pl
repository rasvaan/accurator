:- module(accurator_user, [get_annotated/2,
						   get_annotated_user/2,
						   get_domain/2,
						   get_locale/2,
						   register_user/1,
						   get_user/1,
						   get_user_settings/1,
						   save_user_info/1]).

/** <module> Domain
*/
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(user(user_db)).

:- rdf_register_prefix(edm, 'http://www.europeana.eu/schemas/edm/').
:- rdf_register_prefix(oa, 'http://www.w3.org/ns/oa#').

%%	get_annotated(-Dic, +Options)
%
%	Query for a list of artworks the user recently annotated and return
%	dict
get_annotated(Dic, Options) :-
	option(user(User), Options),
	get_annotated_user(User, Uris),
	Dic = artworks{uris:Uris}.

%%	get_annotated(+User, -AnnotatedUris)
%
%	Query for a list of artworks the user recently annotated.
get_annotated_user(User, Uris) :-
	setof(Uri, Annotation^User^
		  (	    rdf(Annotation, oa:annotatedBy, User),
				rdf(Annotation, oa:hasTarget, Uri),
				rdf(Uri, rdf:type, edm:'ProvidedCHO')),
		  Uris), !.
get_annotated_user(_User, []).

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
save_info_pairs(User, [PropertyString-ValueString|Pairs]) :-
	atom_string(Property, PropertyString),
	atom_string(Value, ValueString),
	InfoAtom =.. [Property, Value],
	set_user_property(User, InfoAtom),
	save_info_pairs(User, Pairs).
