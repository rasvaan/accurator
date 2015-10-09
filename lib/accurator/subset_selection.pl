:- module(subset_selection, [target_iconclass_code/3,
							 target_prefix/3,
							 text_contains_label/5,
							 target_graph/3,
							 target_description_scanner/0,
							 target_title_scanner/0]).

/** <module> Subset selection for annotation
*/
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(thread)).
:- use_module(library(oa_annotation)).

:- rdf_register_prefix(oa, 'http://www.w3.org/ns/oa#').
:- rdf_register_prefix(accu, 'http://accurator.nl/schema#').
:- rdf_register_prefix(edm, 'http://www.europeana.eu/schemas/edm/').

%%	target_iconclass_code(+Code, +TargetType, +Campaign)
%
%	Targets edm works which have an iconclass code which is similair or
%	a subclass to Code and that have an image.
% target_iconclass_code('http://iconclass.org/25F3','http://accurator.nl/bird#Target','http://accurator.nl/bird#Campaign').
target_iconclass_code(Class, TargetType, Campaign) :-
	Options = [target_type(TargetType), campaign(Campaign),
			  targetter('http://accurator.nl/user#ICScanner')],
	%find all works with class or sublcass of specified class
	findall(Work,
			(	subject_subclass_of(Work, Class),
				has_image(Work)),
			ClassWorks),
	length(ClassWorks, NumberClassWorks),
	debug(tag_works, 'Number of works with ~p or lower: ~p',
		  [Class, NumberClassWorks]),
	maplist(campaign_nomination(Options), ClassWorks).

%%	subject_subclass_of(+Work, +Class)
%
%	See if the subject is subclass of a specified class.
subject_subclass_of(Work, Class) :-
	rdf(Work, dc:subject, Subject),
	rdf_reachable(Subject, skos:broader, Class).

%%	target_prefix(+Prefix, +TargetType, +Campaign)
%
%	Targets edm works which have a given prefix
% target_prefix('http://iconclass.org/25F3','http://accurator.nl/bird#Target','http://accurator.nl/bird#Campaign').
target_prefix(Prefix, TargetType, Campaign) :-
	Options = [target_type(TargetType), campaign(Campaign),
			  targetter('http://accurator.nl/user#PrefixScanner')],
	findall(Work,
			(	subject_has_prefix(Work, Prefix),
				has_image(Work)),
			PrefixBirdWorks),
	length(PrefixBirdWorks, NumberPrefixBirds),
	debug(tag_works, 'Number of works with ~p in prefix: ~p',
		  [Prefix, NumberPrefixBirds]),
	maplist(campaign_nomination(Options), PrefixBirdWorks).

%%	subject_has_prefix(+Work, +Prefix)
%
%	See if the subject of a work includes the specified prefix
subject_has_prefix(Work, Prefix) :-
	rdf(Work, dc:subject, Subject),
	atom(Subject),
	atom_prefix(Subject, Prefix).

%%	text_contains_label(+LabelPredicate, +Wildcards, +TargetType,
%	+Campaign, +Graph)
%
%	Retrieves a list of labels linked using the LabelPredicate and
%	checks for every title and description whether the label is present.
%	Wildcards are atoms which can also be scanned for but have no
%	concept assosiated to them.
%
%	text_contains_label('http://lod.taxonconcept.org/ontology/txn.owl#commonName', ['vogel', 'vogels'], 'http://accurator.nl/bird#Target','http://accurator.nl/bird#Campaign', 'http://purl.org/collections/nl/rma/rma-edm-bird-selection.ttl').

text_contains_label(LabelPredicate, Wildcards, TargetType, Campaign, Graph) :-
	Options = [label_predicate(LabelPredicate), wildcards(Wildcards),
			   target_type(TargetType), campaign(Campaign)],
	findall(Work,
			rdf(Work, rdf:type, edm:'ProvidedCHO', Graph),
			AllWorks),
	findall(Label,
			rdf(_Concept, LabelPredicate, literal(lang(nl, Label))),
			Labels0),
	append(Wildcards, Labels0, Labels),
	debug(tag_works, '~p', [Labels]),
	maplist(scan_text_for_birdname(Labels, Options), AllWorks).

%%	scan_text_for_birdname(+Labels, +Options, +Work)
%
%	Scan the title and description field for the precense of labels and
%	nominate and annotate when possible.
scan_text_for_birdname(Labels, Options, Work) :-
	debug(tag_works, 'Scanning ~p', [Work]),
	concurrent_maplist(scan_title(Work, Options), Labels),
	concurrent_maplist(scan_description(Work, Options), Labels).
scan_text_for_birdname(_Labels, _Options, _Work).

%%	scan_title(+Work, +Options, +Label)
%
%	Scan the title for precence of label, by doing a substring matched
%	of the lowercased items.
scan_title(Work, Options0, Label) :-
	Options = [targetter('http://accurator.nl/user#TitleScanner') | Options0],
	atomic_list_concat([' ', Label, ' '], LabelSpaced),
	atom_string(LabelSpaced, LabelString),
	string_lower(LabelString, LabelLower),
	rdf(Work, dc:title, literal(lang(nl, Title))),
	atom_string(Title, TitleString),
	string_lower(TitleString, TitleLower),
	sub_string(TitleLower, _Before, _Length, _After, LabelLower),
	debug(tag_works, 'Title: ~p Label: ~p', [TitleLower, LabelLower]),
	% Check if it has an image, otherwise don't bother
	has_image(Work),
	!,
	add_annotation(Work, Label, Options),
	campaign_nomination(Options, Work),
	debug(scan_text, '~p present in: ~p', [LabelLower, TitleLower]).
scan_title(_, _, _).

%%	add_annotation(+Work, +Name, +Wildcards, +Targetter)
%
%	Add annotatoin based on the title scanner.
add_annotation(_Work, Label, Options) :-
	option(wildcards(Wildcards), Options),
	%Don't add annotation if in wildcards
	member(Label, Wildcards),
	!.
add_annotation(Work, Label, Options) :-
	option(targetter(Targetter), Options),
	atomic_list_concat(['Substring match by ', Targetter, ' in title.'],
					   Motivation),
	get_common_label_uri(Label, Body, Options),
	AnnotationOptions = [user(Targetter),
						 field(dcterms:subject),
						 graph(Targetter),
						 motivatedBy(Motivation),
						 body(_{'@id':Body}),
						 target([_{'@id':Work}]),
						 label(Label),
						 reached_object_with('iteration')],
	rdf_add_annotation(AnnotationOptions, _Annotation),
	debug(add_annotation, 'Added ~p to ~p by ~p', [Label, Work, Targetter]).

%%  get_common_label_uri(+Label, -Uri, +Options)
%
%	Retrieve the Uri of a label.
get_common_label_uri(Label, Uri, Options) :-
	option(label_predicate(Predicate), Options),
	rdf(Uri, Predicate, literal(lang(nl, Label))).

%%	scan_description(+Work, +Options, +Label)
%
%	Scan the description for precence of label, by doing a substring
%	matched of the lowercased items.
scan_description(Work, Options0, Label) :-
	Options = [targetter('http://accurator.nl/user#DescriptionScanner') | Options0],
	atomic_list_concat([' ', Label, ' '], LabelSpaced),
	atom_string(LabelSpaced, LabelString),
	string_lower(LabelString, LabelLower),
	rdf(Work, dc:description, literal(lang(nl, Description))),
	atom_string(Description, DescriptionString),
	string_lower(DescriptionString, DescriptionLower),
	sub_string(DescriptionLower, _Before, _Length, _After, LabelLower),
	% Check if it has an image, otherwise don't bother
	has_image(Work),
	!,
	add_annotation(Work, Label, Options),
	campaign_nomination(Options, Work),
	debug(scan_text, '~p present in: ~p', [LabelLower, DescriptionLower]).
scan_description(_, _, _).

%%	campaign_nomination(+Options, +Work)
%
%	Nominate a work to be in a campaign and record who targetted this
%	work.
campaign_nomination(Options, Work) :-
	option(target_type(TargetType), Options),
	option(targetter(Targetter), Options),
	%Don't assert when already nomminated
	rdf(Work, rdf:type, TargetType),
	rdf(Work, accu:targetedBy, Targetter), !.
campaign_nomination(Options, Work) :-
	option(target_type(TargetType), Options),
	option(targetter(Targetter), Options),
	option(campaign(Campaign), Options),
	rdf_assert(Work, rdf:type, TargetType, Campaign),
	rdf_assert(Work, accu:targetedBy, Targetter, Campaign).

%%	has_image(+Work)
%
%	Check if an object actually has an image.
has_image(Work) :-
	rdf(Aggregation, edm:aggregatedCHO, Work),
	rdf(Aggregation, edm:isShownBy, ImageURL),
	debug(has_image, 'Checking ~p ', [Work]),
	https_header_response(ImageURL, Status),
	sleep(0.2),
	Status == 200,
	debug(has_image, '~p has image.', [Work]).

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

%%	target_graph(+TargetType, +TargetCampaign, +TargetGraph)
%
%	Make the works in a graph targets.
% target_graph('http://accurator.nl/bible#Target', 'http://accurator.nl/bible#Campaign','http://purl.org/collections/nl/ubvu/ubvu_bibles.ttl')
target_graph(TargetType, Campaign, Graph) :-
	Options = [target_type(TargetType), campaign(Campaign),
			  targetter('http://accurator.nl/user#CollectionScanner')],
	%find all works with class or sublcass of specified class
	findall(Work,
			rdf(Work, rdf:type, edm:'ProvidedCHO', Graph),
			ClassWorks),
	length(ClassWorks, NumberClassWorks),
	debug(tag_works, 'Number of objects in graph: ~p',
		  [NumberClassWorks]),
	maplist(campaign_nomination(Options), ClassWorks).

target_title_scanner :-
	Targetter = 'http://accurator.nl/user#TitleScanner',
	TargetType = 'http://accurator.nl/bird#Target',
	Campaign = 'http://accurator.nl/bird#Campaign',
	Options = [target_type(TargetType), campaign(Campaign),
			  targetter(Targetter)],
	findall(Work,
			(	rdf(Annotation, oa:annotatedBy, Targetter),
				rdf(Annotation, oa:hasTarget, Work) ),
			ClassWorks),
	length(ClassWorks, NumberClassWorks),
	debug(tag_works, 'Number scanned: ~p',
		  [NumberClassWorks]),
	maplist(campaign_nomination(Options), ClassWorks).

target_description_scanner :-
	Targetter = 'http://accurator.nl/user#DescriptionScanner',
	TargetType = 'http://accurator.nl/bird#Target',
	Campaign = 'http://accurator.nl/bird#Campaign',
	Options = [target_type(TargetType), campaign(Campaign),
			  targetter(Targetter)],
	findall(Work,
			(	rdf(Annotation, oa:annotatedBy, Targetter),
				rdf(Annotation, oa:hasTarget, Work) ),
			ClassWorks),
	length(ClassWorks, NumberClassWorks),
	debug(tag_works, 'Number scanned: ~p',
		  [NumberClassWorks]),
	maplist(campaign_nomination(Options), ClassWorks).
