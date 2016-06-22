:- module(annotation, [
			  annotation_fields/2,
			  annotations/3,
			  annotations/4,
			  number_of_annotations/2,
			  number_of_users/2,
			  enrich_annotations/3
		  ]).

:- use_module(library(accurator/ui_elements)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdfs)).

:- rdf_meta
	fields(r,-,-,-),
	label_property_pair(-, r, -, -),
	annotations(-,r,-).

%%	annotation_fields(-Fields, +Options)
%
%	Retrieve the definitions of annotation fields from the triple store
%	and add to dict
annotation_fields(Fields, Options) :-
	option(annotation_ui(UI), Options),
	option(locale(Locale), Options),
	fields(auis:fragmentFields, Locale, UI, FragmentFields),
	fields(auis:wholeFields, Locale, UI, WholeFields),
	Fields = fields{fragment_fields:FragmentFields, whole_fields: WholeFields}.

%%	fields(+Type, +Locale, +UI, -Fields)
%
%	Retrieve ordered list of fields for describing fragments
fields(Type, Locale, UI, Fields) :-
	rdf_has(UI, Type, RdfList), !,
	rdfs_list_to_prolog_list(RdfList, FieldUris),
	maplist(get_field(Locale), FieldUris, Fields).
fields(_Type, _Locale, _UI, []).

%%	get_field(+Locale, +UI, -Fields)
%
%	Determine type of field and get the appropriate dict
get_field(Locale, Uri, FieldDict) :-
	rdf(Uri, rdf:type, FieldTypeUri),
	iri_xml_namespace(FieldTypeUri, _, FieldType),
	field(FieldType, Uri, Locale, FieldDict).

%%	field(+Type, +Uri, +Locale, -FieldDict)
%
%	Get info according to the specified type of field
field('DropdownField', Uri, Locale, Field) :-
	field_description(Uri, Locale, Properties),
	get_source(Uri, Locale, Source), !,
	dict_pairs(Field, elements, ['source'-Source|Properties]).
field('SelectField', Uri, Locale, Field) :-
	field_description(Uri, Locale, Properties),
	get_source(Uri, Locale, Source), !,
	dict_pairs(Field, elements, ['source'-Source|Properties]).
field('TextField', Uri, Locale, Field) :-
	field_description(Uri, Locale, Properties),	!,
	dict_pairs(Field, elements, Properties).
field('RadioButtonField', Uri, Locale, Field) :-
	field_description(Uri, Locale, Properties),
	get_source(Uri, Locale, Source), !,
	dict_pairs(Field, elements, ['source'-Source|Properties]).
field('CheckboxField', Uri, Locale, Field) :-
	field_description(Uri, Locale, Properties),
	get_source(Uri, Locale, Source), !,
	dict_pairs(Field, elements, ['source'-Source|Properties]).
field(_, Uri, Locale, Field) :-
	field_description(Uri, Locale, Properties),
	dict_pairs(Field, elements, Properties).

%%	field_description(+Uri, +Locale, -DescriptionList)
%
%	Get the label and comment textually describing the field
field_description(Uri, Locale, ['uri'-Uri, 'type'-FieldType, LabelPair, CommentPair]) :-
	rdf(Uri, rdf:type, FieldTypeUri),
	iri_xml_namespace(FieldTypeUri, _, FieldType),
	label_property_pair(Uri, rdfs:label, Locale, LabelPair),
	label_property_pair(Uri, dcterms:comment, Locale, CommentPair).

%%	label_property_pair(+Uri, +Property, +Locale, -PropertyValuePair)
%
%	Get the text preferrably in the preferred language and shorten the
%	property label
label_property_pair(Uri, Property, Locale, UriLabel-Value) :-
	get_literal(Uri, Property, Locale, Value),
	iri_xml_namespace(Property, _, UriLabel).

get_literal(Subject, Predicate, Locale, Literal) :-
	rdf(Subject, Predicate, literal(lang(Locale, Literal))), !.
get_literal(Subject, Predicate, _Locale, Literal) :-
	rdf(Subject, Predicate, literal(lang(en, Literal))), !.
get_literal(Subject, Predicate, _Locale, Literal) :-
	rdf(Subject, Predicate, literal(lang(_L, Literal))), !.
get_literal(Subject, Predicate, _Locale, Literal) :-
	rdf(Subject, Predicate, literal(Literal)), !.

%%	get_source(+Uri, +Locale, -Source)
%
%	Get the source, either in the form of a list of alternatives
%	extracted according to specified language, or literal defining the
%	autocompletion parameters.
get_source(Uri, Locale, Source) :-
	rdf_has(Uri, auis:source, RdfList),
	rdfs_list_to_prolog_list(RdfList, SourceList),
	maplist(extract_literal(Locale), SourceList, Source), !.
get_source(Uri, _Locale, Source) :-
	rdf_has(Uri, auis:source, SourceUri),
	rdf_is_resource(SourceUri), !,
	findall(PredicateLabel-Literal,
			(	rdf(SourceUri, Predicate, literal(Literal)),
				iri_xml_namespace(Predicate, _, PredicateLabel)),
			Properties),
	dict_pairs(Source, elements, Properties).

extract_literal(Locale, literal(lang(Locale, Literal)), Literal) :- !.
extract_literal(_Locale, literal(lang(en, Literal)), Literal) :- !.
extract_literal(_Locale, literal(Literal), Literal) :-
	atom(Literal), !. % check if not lang(Literal)

%%	annotations(+Type, +Uri, -Metadata)
%
%	Get all annotations and subjects attached to a Uri, get all
%	annotations a user made, or get all annotations of which the body
%	are of in specified concept scheme or domain.
% annotations(domain,'http://accurator.nl/fashion/jewelry#domain', Annotations). annotations(concept_scheme,
% 'http://purl.org/vocab/nl/ubvu/BiblePageConceptScheme', Annotations).

annotations(object, Uri, Annotations) :-
	findall(annotation{
				field:FieldLabel,
				body:NiceBody},
	    (	get_annotation(Uri, AnnotationBody, AnnotationHash),
			process_annotation(AnnotationBody, NiceBody),
			% use rdf_has to also include fragment and whole fields
			rdf_has(AnnotationHash, auis:fields, FieldUri),
			rdf_display_label(FieldUri, _, FieldLabel)
	    ),
	    FoundAnnotations),
	get_title(Uri, DisplayTitle),
	Annotations = annotations{title:DisplayTitle, annotations:FoundAnnotations}.

annotations(user, UserUri, ObjectUris) :-
    setof(Object, AnnotationHash^
	    (	rdf_has(AnnotationHash, oa:annotatedBy, UserUri),
			rdf_has(AnnotationHash, oa:hasTarget, Object),
			rdf(Object, rdf:type, edm:'ProvidedCHO')	    ),
	    ObjectUris), !.
annotations(user, _UserUri, []).

annotations(domain, Domain, Annotations) :-
	findall(AnnotationHash,
			(	rdf(AnnotationHash, oa:hasTarget, Work),
				rdf(Work, rdf:type, Target),
				rdf(Domain, 'http://accurator.nl/schema#hasTarget', Target)),
			Annotations).

annotations(concept_scheme, ConceptScheme, Annotations) :-
	findall(AnnotationHash,
			(	rdf(AnnotationHash, oa:hasBody, AnnotationBody),
				rdf(AnnotationBody, skos:inScheme, ConceptScheme)),
			Annotations).
annotations(concept_scheme, ConceptScheme, Annotations, Graph) :-
	findall(AnnotationHash,
			(	rdf(AnnotationHash, oa:hasBody, AnnotationBody),
				rdf(AnnotationHash, oa:hasTarget, Uri),
				rdf(Uri, _P, _O, Graph),
				rdf(AnnotationBody, skos:inScheme, ConceptScheme)),
			Annotations).

get_annotation(Uri, AnnotationBody, AnnotationHash) :-
	rdf(AnnotationHash, oa:hasTarget, Uri),
	rdf(AnnotationHash, oa:hasBody, AnnotationBody).

process_annotation(literal(Annotation), Annotation) :- !.
process_annotation(Annotation, Label) :-
	rdf_display_label(Annotation, _, Label).

%%	number_of_annotations(+Uri, -NumberOfAnnotations)
%
%	Get the number of annotations for a given uri
number_of_annotations(Uri, NumberOfAnnotations) :-
	setof(Annotation,
		  rdf(Annotation, oa:hasTarget, Uri),
		  Annotations), !,
	length(Annotations, NumberOfAnnotations).
number_of_annotations(_Uri, 0) :- !.

%%	number_of_users(+Uri, -NumberOfUsers)
%
%	Get the number of users who annotated the object of the given uri.
number_of_users(Uri, NumberOfUsers) :-
	setof(User,
		  (	    rdf(Annotation, oa:hasTarget, Uri),
				rdf(Annotation, oa:annotatedBy, User)),
		  Users), !,
	length(Users, NumberOfUsers).
number_of_users(_Uri, 0) :- !.

%%	enrich_annotatoins(+Enrich, +Annotations, +EnrichedAnnotations)
%
%	Enrich the annotations with additional information.
enrich_annotations(false, Annotations, Annotations).
enrich_annotations(true, Annotations, EnrichedAnnotations) :-
	maplist(enrich_annotation, Annotations, EnrichedAnnotations).


%%	enrich_annotation(+AnnotationUri, -EnrichedAnnotation)
%
%	Based on the annotation uri, retrieve additional information.
enrich_annotation(AnnotationUri, EnrichedAnnotation) :-
	annotation_label(AnnotationUri, Label),
	annotated_object(AnnotationUri, ObjectUri, ObjectTitle),
	type_annotation(AnnotationUri, Type),
	EnrichedAnnotation = _{uri:AnnotationUri,
						   label:Label,
						   type:Type,
						   object:_{uri:ObjectUri, title:ObjectTitle}}.

annotation_label(AnnotationUri, Label) :-
	rdf(AnnotationUri, dcterms:title, literal(Label)),
	atom(Label), !.
annotation_label(_AnnotationUri, no_title).

annotated_object(AnnotationUri, ObjectUri, ObjectTitle) :-
	rdf(AnnotationUri, oa:hasTarget, ObjectUri),
	rdf(ObjectUri, rdf:type, edm:'ProvidedCHO'),
	get_literal(ObjectUri, dc:title, en, ObjectTitle), !.

annotated_object(AnnotationUri, ObjectUri, no_title) :-
	rdf(AnnotationUri, oa:hasTarget, ObjectUri),
	rdf(ObjectUri, rdf:type, edm:'ProvidedCHO'), !.

annotated_object(_AnnotationUri, no_uri, no_title).


type_annotation(AnnotationUri, text) :-
	rdf(AnnotationUri, oa:hasBody, BlancNode),
	rdf(BlancNode, rdf:type, cnt:'ContentAsText'), !.

type_annotation(AnnotationUri, concept) :-
	rdf(AnnotationUri, oa:hasBody, Annotation),
	rdf_is_resource(Annotation), !.

type_annotation(_AnnotationUri, unknown) :-
	!.
