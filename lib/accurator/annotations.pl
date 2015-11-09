:- module(annotations, [
			  annotations/3
		  ]).

:- use_module(library(accurator/ui_elements)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).

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
	Annotations = annotations{display_title:DisplayTitle, annotations:FoundAnnotations}.
annotations(user, UserUri, ObjectUris) :-
    setof(Object, AnnotationHash^Selector^
	    (	rdf_has(AnnotationHash, oa:annotatedBy, UserUri),
			rdf_has(AnnotationHash, oa:hasTarget, Selector),
			rdf_has(Selector, oa:hasSource, Object)	    ),
	    ObjectUris).

get_annotation(Uri, AnnotationBody, AnnotationHash) :-
	rdf(AnnotationHash, oa:hasTarget, Uri),
	rdf(AnnotationHash, oa:hasBody, AnnotationBody).

process_annotation(literal(Annotation), Annotation) :- !.
process_annotation(Annotation, Label) :-
	rdf_display_label(Annotation, _, Label).


