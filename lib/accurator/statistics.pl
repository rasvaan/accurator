:- module(statistics, [annotation_statistics/2]).

:- use_module(library(accurator/annotation)).
:- use_module(library(accurator/domain)).


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
	number_annotations(Domain, NumberAnnotations),
	reviewed_annotations(Domain, NumberReviewed),
	active_annotators(Domain, NumberAnnotators),
	Statistics = _{number_annotations:NumberAnnotations,
				   reviewed_annotations:NumberReviewed,
				   annotators:NumberAnnotators}.

%%	number_annotations(+Domain, -NumberAnnotations)
%
%	Retrieve the number of annotations made in the domain.
number_annotations(Domain, NumberAnnotations) :-
	domain_uri(Domain, DomainUri),
	annotations(domain, DomainUri, Annotations),
	length(Annotations, NumberAnnotations).

%%	reviewed_annotations(+Domain, -NumberReviewed)
%
%	Number of annotations which have been reviewed.
reviewed_annotations(_Domain, 0).

%%	active_annotators(+Domain, -NumberAnnotators)
%
%	The number of annotators in active in this domain.
active_annotators(Domain, NumberAnnotators) :-
	domain_uri(Domain, DomainUri),
	annotations(domain, DomainUri, Annotations),
	maplist(annotation_created_by, Annotations, Users),
	list_to_set(Users, UserSet),
	length(UserSet, NumberAnnotators).

% helper function for retrieving user uris.
annotation_created_by(AnnotationHash, User) :-
	rdf(AnnotationHash, oa:annotatedBy, User).
