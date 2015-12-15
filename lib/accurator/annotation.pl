:- module(annotation, [
			  annotation_fields/2,
			  annotations/3
		  ]).

:- use_module(library(accurator/ui_elements)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdfs)).
:- use_module(user(user_db)).

%%	annotation_fields(-Fields, +Options)
%
%	Retrieve the definitions of annotation fields from the triple store
annotation_fields(Fields, Options) :-
	get_annotation_fields(Fields, Options).

get_annotation_fields(Fields, Options) :-
	option(annotation_ui(UI), Options),
	rdf_has(UI, ann_ui:fields, RdfList),
	rdfs_list_to_prolog_list(RdfList, Fields).

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
