:- module(subset_selection, [tag_birds/0]).

/** <module> Subset selection for annotation
*/
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(oa_annotation)).

:- rdf_register_prefix(abir, 'http://accurator.nl/bird#').

tag_birds :-
	%find all works with class or sublcass of specified class
	findall(Work,
			subject_subclass_of(Work, 'http://iconclass.org/25F3'),
			%has_image(Work),
			ClassBirdWorks),
	length(ClassBirdWorks, NumberClassBirds),
	debug(tag_works, 'Number of works with 25F3 or lower: ~p', [NumberClassBirds]),
	findall(Work,
			subject_has_prefix(Work, 'http://iconclass.org/25F3'),
			PrefixBirdWorks),
	length(PrefixBirdWorks, NumberPrefixBirds),
	debug(tag_works, 'Number of works with 25F3 in prefix: ~p', [NumberPrefixBirds]),
	findall(Work,
			rdf(Work, rdf:type, edm:'ProvidedCHO'),
			AllWorks),
	findall(Label,
			rdf(_Label, 'http://lod.taxonconcept.org/ontology/txn.owl#commonName', literal(lang(nl, Label))),
			Labels),
	Labels2=['vogel'| Labels],
	Labels2=['vogels'| Labels],
	debug(tag_works, '~p', [Labels3]),
	maplist(scan_text_for_birdname(Labels3), AllWorks).

scan_text_for_birdname(Labels, Work) :-
	maplist(scan_description(Work), Labels).
	%maplist(scan_title(Work), Labels).

scan_title(Work, Name) :-
	debug(scan_label, '~p', [Name]),
	atomic_list_concat([' ', Name, ' '], NameSpaced),
	atom_string(NameSpaced, NameString),
	string_lower(NameString, NameLower),
	rdf(Work, dc:title, literal(lang(nl, Title))),
	atom_string(Title, TitleString),
	string_lower(TitleString, TitleLower),
	sub_string(TitleLower, _Before, _Length, _After, NameLower),
	!,
	add_annotation_title(Work, Name),
	debug(scan_text, '~p present in: ~p', [NameLower, TitleLower]).
scan_title(_, _).

add_annotation_title(_Work, Name) :-
	%Don't add annotation if vogel or vogels is named
	member(Name, ['vogel', 'vogels']).
add_annotation_title(Work, Name) :-
	atomic_list_concat(['Substring match of ', Name, ' in title.'], Motivation),
	get_common_label_uri(Name, Body),
	Options = [user('http://accurator.nl/user#TitleScanner'),
			   field(dcterms:subject),
			   graph('http://accurator.nl/user#TitleScanner'),
			   motivatedBy(Motivation),
			   body(Body),
			   target(Work)],
	rdf_add_annotation(Options, _Annotation).

get_common_label_uri(Label, Uri) :-
	rdf(Uri,
		'http://lod.taxonconcept.org/ontology/txn.owl#commonName',
		literal(lang(nl, Label))).

scan_description(Work, Name) :-
	debug(scan_label, '~p', [Name]),
	atomic_list_concat([' ', Name, ' '], NameSpaced),
	atom_string(NameSpaced, NameString),
	string_lower(NameString, NameLower),
	rdf(Work, dc:description, literal(lang(nl, Title))),
	atom_string(Title, TitleString),
	string_lower(TitleString, TitleLower),
	sub_string(TitleLower, _Before, _Length, _After, NameLower),
	!,
	add_annotation_description(Work, Name),
	campaign_nomination(Work, abir:'Target', 'http://accurator.nl/user#DescriptionScanner'),
	debug(scan_text, '~p present in: ~p', [NameLower, TitleLower]).
scan_description(_, _).

campaign_nomination(Work, Type, User) :-
	%Don't assert when already nomminated
	rdf(Work, rdf:type, Type),
	rdf(Work, accu:targetedBy, User), !.
campaign_nomination(Work, Type, User) :-
	rdf_assert(Work, rdf:type, Type, abir:'Campaign'),
	rdf(Work, accu:targetedBy, User, abir:'Campain').

add_annotation_description(_Work, Name) :-
	%Don't add annotation if vogel or vogels is named
	member(Name, ['vogel', 'vogels']).
add_annotation_description(Work, Name) :-
	atomic_list_concat(['Substring match of ', Name, ' in title.'], Motivation),
	get_common_label_uri(Name, Body),
	Options = [user('http://accurator.nl/user#DescriptionScanner'),
			   field(dcterms:subject),
			   graph('http://accurator.nl/user#DescriptionScanner'),
			   motivatedBy(Motivation),
			   body(Body),
			   target(Work)],
	rdf_add_annotation(Options, _Annotation).

has_image(Work) :-
	rdf(Aggregation, edm:aggregatedCHO, Work),
	rdf(Aggregation, edm:isShownBy, ImageURL),
	https_header_response(ImageURL, Status),
	sleep(0.2),
    Status == 200.

subject_has_prefix(Work, Prefix) :-
	rdf(Work, dc:subject, Subject),
	atom(Subject),
	atom_prefix(Subject, Prefix).

subject_subclass_of(Work, Class) :-
	rdf(Work, dc:subject, Subject),
	rdf_reachable(Subject, skos:broader, Class).

%%	https_header_response(+URL, -Status)
%
%	Return the response header of input URL.
https_header_response(URL, Status) :-
    http_open(URL, In,
	  [method(head),
	   status_code(Status),
	   cert_verify_hook(ssl_verify)
	  ]),
    close(In).

:- public ssl_verify/5.

%%	ssl_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
%
%	Currently we accept  all  certificates.
ssl_verify(_SSL, _ProblemCertificate,
		   _AllCertificates, _FirstCertificate,
		   _Error).
