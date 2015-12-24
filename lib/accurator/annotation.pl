:- module(annotation, [
			  annotation_fields/2,
			  annotations/3
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
	field_description(Uri, Locale, Properties),	!,
	dict_pairs(Field, elements, Properties).
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
field_description(Uri, Locale, [LabelPair, CommentPair]) :-
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
	rdf_has(Uri, auis:source, literal(Source)), !.

extract_literal(Locale, literal(lang(Locale, Literal)), Literal) :- !.
extract_literal(_Locale, literal(lang(en, Literal)), Literal) :- !.

%%	annotations(+Type, +Uri, -Metadata)
%
%	Get all annotations and subjects attached to a Uri, or get all
%	annotations a user made.
annotations(object, Uri, Annotations) :-
    findall(annotation{
		     field:FieldLabel,
		     body:NiceBody},
	    (	get_annotation(Uri, AnnotationBody, AnnotationHash),
			process_annotation(AnnotationBody, NiceBody),
			rdf(AnnotationHash, ann_ui:annotationField, FieldUri),
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

get_annotation(Uri, AnnotationBody, AnnotationHash) :-
	rdf(AnnotationHash, oa:hasTarget, Uri),
	rdf(AnnotationHash, oa:hasBody, AnnotationBody).

process_annotation(literal(Annotation), Annotation) :- !.
process_annotation(Annotation, Label) :-
	rdf_display_label(Annotation, _, Label).
