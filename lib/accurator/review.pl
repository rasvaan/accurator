:- module(review, [
			  review/3,
			  reviews/3,
			  process_annotations/0,
			  select_annotations/3,
			  agreeable_annotations/0]).

:- use_module(library(oa_annotation)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(accurator/annotation)).

:- rdf_meta
	review(r, -, r).

%%	reviews(+Uris, +User, +Uris)
%
%	Add a list of reviews.
%
%	* Judgement - should be agree/disagree/unsure
%
%	reviews(agree,'http://accurator.nl/user#r', ['http://localhost/annotation/instances/id_28d9b294881ca09b72c6d0a1eeaa25015d50f860','http://localhost/annotation/instances/id_897ed7aa0034be64512cb1091504dcf7910ddb46']).

reviews(Judgement, User, Uris) :-
	maplist(review(Judgement, User), Uris).

%%	review(+Judgement, +User, +Uri)
%
%	Add a review annotation with a judgement by a specified user.
review(Judgement, User, Uri) :-
	member(Judgement, [agree,disagree,unsure]), !,% verify value
	atom_string(Judgement, JudgementString),
	atom_string(Uri, UriString),
	Options = [
		target([_{'@id':UriString}]), % we don't do simple..
		label(Judgement),
		body(_{'@value':JudgementString}),
		user(User),
		motivatedBy('http://www.w3.org/ns/oa#moderating')
	],
	debug(review, 'Add review: ~p', [Options]),
    rdf_add_annotation(Options, _Annotation).


process_annotations :-
	select_annotations('http://purl.org/vocab/nl/ubvu/BiblePageConceptScheme','http://accurator.nl/user#rasvaan',Annotations),
	generate_graph_name(Annotations, Graph),
	export_annotations(Graph, Annotations).

%%	generate_graph_name(UriList, Hash)
%
%	Generate a hash which can be used as a name for a graph based on a
%	list of uris, making it relatively unique.
generate_graph_name(UriList, Hash) :-
	sort(UriList, SortedUris),
	variant_sha1(SortedUris, Hash).

%%	select_annotations(+ConceptScheme, +User, -Annotations)
%
%	Select a list of annotations based upon given User and
%	ConceptScheme.
%	select_annotations('http://purl.org/vocab/nl/ubvu/BiblePageConceptScheme','http://accurator.nl/user#rasvaan',Annotations).
select_annotations(ConceptScheme, User, Annotations) :-
	setof(Annotation, Review^BlankReviewNode^AnnotationBody^
			(	rdf(Review, oa:annotatedBy, User),
				rdf(Review, oa:hasBody, BlankReviewNode),
				rdf(BlankReviewNode, cnt:chars, literal('agree')),
				rdf(Review, oa:hasTarget, Annotation),
				rdf(Annotation, oa:hasBody, AnnotationBody),
				rdf(AnnotationBody, skos:inScheme, ConceptScheme)),
			Annotations),
	length(Annotations, Number),
	format('Selected ~p annotations.', [Number]).


%%	export_annotations(Annotations)
%
%	Export a list of annotations
export_annotations(Graph, Annotations) :-
	rdf_unload_graph(Graph),
	maplist(add_annotation(Graph), Annotations).

add_annotation(Graph, Annotation) :-
	% get all triples related to annotation
	findall(triple(Annotation, Predicate, Object),
			rdf(Annotation, Predicate, Object),
			AnnotationTriples),
	maplist(assert_tiple(Graph), AnnotationTriples),
	length(AnnotationTriples, NumberTriples),
	format('Added ~p triples to ~p~n', [NumberTriples, Graph]).

assert_tiple(Graph, triple(Subject, Predicate, Object)) :-
	rdf_assert(Subject, Predicate, Object, Graph).

%%	agreeable_annotations
%
%	Agree on an agreeable set of annotations
agreeable_annotations :-
	% get annotations ubvu based on conceptscheme
	 annotations(concept_scheme, 'http://purl.org/vocab/nl/ubvu/BiblePageConceptScheme', Annotations),
	 length(Annotations, Number),
	 format('~p Annotations are being agreed upon', [Number]),
	 reviews(agree,'http://accurator.nl/user#rasvaan', Annotations).
