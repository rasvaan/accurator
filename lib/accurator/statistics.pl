:- module(statistics, [annotation_statistics/2]).

:- use_module(library(accurator/annotation)).
:- use_module(library(accurator/domain)).
:- use_module(library(semweb/rdf_db)).


%%	annotation_statics(-Statistics, +Options)
%
%	Determine which statitics to obtain.
annotation_statistics(Statistics, Options) :-
	option(domain(Domain), Options),
	atom(Domain), !,
	domain_statistics(Domain, Statistics).

%%	domain_statistics(+Domain, -Statistics)
%
%	Agree on an agreeable set of annotations
domain_statistics(Domain, Statistics) :-
	domain_uri(Domain, DomainUri),
	annotations(domain, DomainUri, Annotations),
	length(Annotations, NumberAnnotations),
	objects_annotated(Annotations, NumberObjects),
	number_reviewed_annotations(Annotations, NumberReviewed),
	active_annotators(Annotations, NumberAnnotators),
	Statistics = _{number_annotations:NumberAnnotations,
				   objects_annotated:NumberObjects,
				   reviewed_annotations:NumberReviewed,
				   annotators:NumberAnnotators}.

%%	objects_annotated(+Annotations, -NumberObjects)
%
%	The number of objects that are annotated in this domain.
objects_annotated(Annotations, NumberObjects) :-
	maplist(annotation_object, Annotations, ObjectList),
	list_to_set(ObjectList, Objects),
	length(Objects, NumberObjects).

% helper function for retrieving object uris
annotation_object(AnnotationHash, Object) :-
	rdf(AnnotationHash, oa:hasTarget, Object),
	rdf(Object, rdf:type, edm:'ProvidedCHO').

%%	number_reviewed_annotations(+Annotations, -NumberReviewed)
%
%	Number of annotations which have been reviewed.
number_reviewed_annotations(Annotations, NumberReviewed) :-
	reviewed_annotations(Annotations, ReviewedAnnotations),
	length(ReviewedAnnotations, NumberReviewed).

%%	reviewed_annotations(+Annotations, -ReviewedAnnotations)
%
%	Scans a list of annotations and returns the annotations which have
%	been reviewed.
reviewed_annotations([], []) :- !.
reviewed_annotations([AnnotationHash|Annotations], [AnnotationHash | Reviewed]) :-
	rdf(Review, oa:hasTarget, AnnotationHash),
	rdf(Review, oa:motivatedBy, oa:moderating), !,
	reviewed_annotations(Annotations, Reviewed).
reviewed_annotations([_AnnotationHash|Annotations], Reviewed) :-
	reviewed_annotations(Annotations, Reviewed).



%%	active_annotators(+Annotations, -NumberAnnotators)
%
%	The number of annotators in active in this domain.
active_annotators(Annotations, NumberAnnotators) :-
	maplist(annotation_created_by, Annotations, Users),
	list_to_set(Users, UserSet),
	length(UserSet, NumberAnnotators).

% helper function for retrieving user uris.
annotation_created_by(AnnotationHash, User) :-
	rdf(AnnotationHash, oa:annotatedBy, User).
