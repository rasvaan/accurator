:- module(review, [
			  review/4,
			  reviews/4,
			  select_annotations/3,
			  select_annotations/4,
			  process_annotations/2,
			  process_annotations/3,
			  agreeable_annotations/2,
			  annotation_reviews/2
		  ]).

:- use_module(library(oa_annotation)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(accurator/annotation)).
:- use_module(library(semweb/rdf_turtle_write)).

:- rdf_meta
	review(r, -, r, -).

%%	annotation_reviews(+AnnotationUri, -Reviews)
%
%	Retrieve the uris of the reviews of an annotation
annotation_reviews(AnnotationUri, Reviews) :-
	setof(Review,
		  (	  rdf(Review, oa:hasTarget, AnnotationUri),
			  rdf(Review, oa:motivatedBy, oa:moderating)),
		  Reviews), !.
annotation_reviews(_AnnotationUri, []).

%%	reviews(+Uris, +User, +Uris, +Graph)
%
%	Add a list of reviews.
%
%	* Judgement - should be agree/disagree/unsure
%
%	reviews(agree,'http://accurator.nl/user#r', ['http://localhost/annotation/instances/id_28d9b294881ca09b72c6d0a1eeaa25015d50f860','http://localhost/annotation/instances/id_897ed7aa0034be64512cb1091504dcf7910ddb46']).

reviews(Judgement, User, Uris, Graph) :-
	maplist(review(Judgement, User, Graph), Uris).

%%	review(+Judgement, +User, +Uri, +Graph)
%
%	Add a review annotation with a judgement by a specified user.
review(Judgement, User, Graph, Uri) :-
	member(Judgement, [agree,disagree,unsure]), !,% verify value
	atom_string(Judgement, JudgementString),
	atom_string(Uri, UriString),
	Options = [
		target([_{'@id':UriString}]), % we don't do simple..
		label(Judgement),
		body(_{'@value':JudgementString}),
		user(User),
		motivatedBy('http://www.w3.org/ns/oa#moderating'),
		graph(Graph)
	],
	debug(review, 'Add review: ~p', [Options]),
    rdf_add_annotation(Options, _Annotation).

%%	process_annotations(+Type, +Uri, +Reviewer)
%
%	Retrieves and saves selection of annotations based on the given
%	concept scheme or domain.
%	process_annotations(domain, 'http://accurator.nl/bible#domain').
process_annotations(Type, Uri) :-
	select_annotations(Type, Uri, Annotations),
	generate_graph_name(Annotations, Graph),
	export_annotations(Graph, Annotations).

%%	process_annotations(+Type, +Uri, +Reviewer)
%
%	Retrieves and saves selection of annotations based on the given
%	concept scheme or domain.
%	process_annotations(domain, 'http://accurator.nl/fashion/jewelry#domain','http://accurator.nl/user#rasvaan').
%	process_annotations(domain, 'http://accurator.nl/bible#domain','http://accurator.nl/user#rasvaan').
process_annotations(Type, Uri, Reviewer) :-
	select_annotations(Type, Uri, Reviewer, Annotations),
	generate_graph_name(Annotations, Graph),
	export_annotations(Graph, Annotations).

%%	generate_graph_name(UriList, Hash)
%
%	Generate a hash which can be used as a name for a graph based on a
%	list of uris, making it relatively unique.
generate_graph_name(UriList, Hash) :-
	sort(UriList, SortedUris),
	variant_sha1(SortedUris, Hash).

%%	select_annotations(+Type, +TypeUri, -Annotations)
%
%	Select a list of annotations based upon ConceptScheme or domain
%	select_annotations(conceptScheme,'http://purl.org/vocab/nl/ubvu/BiblePageConceptScheme',Annotations).
%	select_annotations(domain,'http://accurator.nl/bible#domain',Annotations).
select_annotations(conceptScheme, ConceptScheme, Annotations) :-
	setof(Annotation,
		  verified_annotation(concept_scheme, ConceptScheme, Annotation),
		  Annotations),
	length(Annotations, Number),
	format('Selected ~p annotations.', [Number]).

select_annotations(domain, Domain, Annotations) :-
	setof(Annotation,
		  verified_annotation(domain, Domain, Annotation),
		  Annotations),
	length(Annotations, Number),
	format('Selected ~p annotations.', [Number]).

verified_annotation(concept_scheme, ConceptScheme, Annotation) :-
	rdf(Review, oa:hasBody, BlankReviewNode),
	rdf(BlankReviewNode, cnt:chars, literal('agree')),
	rdf(Review, oa:hasTarget, Annotation),
	rdf(Annotation, oa:hasBody, AnnotationBody),
	rdf(AnnotationBody, skos:inScheme, ConceptScheme).

verified_annotation(domain, Domain, Annotation) :-
	rdf(Review, oa:hasBody, BlankReviewNode),
	rdf(BlankReviewNode, cnt:chars, literal('agree')),
	rdf(Review, oa:hasTarget, Annotation),
	rdf(Annotation, oa:hasTarget, Work),
	rdf(Work, rdf:type, Target),
	rdf(Domain, 'http://accurator.nl/schema#hasTarget', Target).

%%	select_annotations(+Type, +ConceptScheme, +User, -Annotations)
%
%	Select a list of annotations based upon given User and
%	ConceptScheme.
%	select_annotations(conceptScheme,'http://purl.org/vocab/nl/ubvu/BiblePageConceptScheme','http://accurator.nl/user#rasvaan',Annotations).
select_annotations(conceptScheme, ConceptScheme, User, Annotations) :-
	setof(Annotation,
		  user_verified_annotation(concept_scheme, ConceptScheme, User, Annotation),
		  Annotations),
	length(Annotations, Number),
	format('Selected ~p annotations.', [Number]).

select_annotations(domain, Domain, User, Annotations) :-
	setof(Annotation,
		  user_verified_annotation(domain, Domain, User, Annotation),
		  Annotations),
	length(Annotations, Number),
	format('Selected ~p annotations.', [Number]).

user_verified_annotation(concept_scheme, ConceptScheme, User, Annotation) :-
	rdf(Review, oa:annotatedBy, User),
	rdf(Review, oa:hasBody, BlankReviewNode),
	rdf(BlankReviewNode, cnt:chars, literal('agree')),
	rdf(Review, oa:hasTarget, Annotation),
	rdf(Annotation, oa:hasBody, AnnotationBody),
	rdf(AnnotationBody, skos:inScheme, ConceptScheme).

user_verified_annotation(domain, Domain, User, Annotation) :-
	rdf(Review, oa:annotatedBy, User),
	rdf(Review, oa:hasBody, BlankReviewNode),
	rdf(BlankReviewNode, cnt:chars, literal('agree')),
	rdf(Review, oa:hasTarget, Annotation),
	rdf(Annotation, oa:hasTarget, Work),
	rdf(Work, rdf:type, Target),
	rdf(Domain, 'http://accurator.nl/schema#hasTarget', Target).

%%	export_annotations(Annotations)
%
%	Export a list of annotations.
export_annotations(Graph, Annotations) :-
	maplist(add_annotation(Graph), Annotations),
	rdf_save_turtle(Graph, [graph(Graph)]),
	rdf_unload_graph(Graph).

%%	add_annotation(Graph, Annotation)
%
%	Assert triples in Graph describing Annotation
add_annotation(Graph, Annotation) :-
	rdf(Annotation, oa:hasBody, TextNode),
	rdf_is_bnode(TextNode), !,
	% get all triples related to text annotation
	findall(triple(TextNode, Predicate, Object),
			rdf(TextNode, Predicate, Object),
			TextTriples),
	findall(triple(Annotation, Predicate, Object),
			rdf(Annotation, Predicate, Object),
			AnnotationTriples),
	get_target(Annotation, TargetTriples),
	append([AnnotationTriples, TextTriples, TargetTriples], Triples),
	maplist(assert_tiple(Graph), Triples),
	length(AnnotationTriples, NumberTriples),
	format('Added ~p triples to ~p~n', [NumberTriples, Graph]).

add_annotation(Graph, Annotation) :-
	% get all triples related to annotation
	findall(triple(Annotation, Predicate, Object),
			rdf(Annotation, Predicate, Object),
			AnnotationTriples),
	get_target(Annotation, TargetTriples),
	append([AnnotationTriples, TargetTriples], Triples),
	maplist(assert_tiple(Graph), Triples),
	length(AnnotationTriples, NumberTriples),
	format('Added ~p triples to ~p~n', [NumberTriples, Graph]).

%%	get_target(+Annotation, -SelectorTriples)
%
%	For a given annotation get a list of triples describing the
%	selector.
get_target(Annotation, Triples) :-
	rdf(Annotation, oa:hasTarget, Target),
	rdf(Target, rdf:type, oa:'SpecificResource'), !,
	findall(triple(Target, Predicate, Object),
			rdf(Target, Predicate, Object),
			TargetTriples),
	% get selector triples
	rdf(Target, oa:hasSelector, Selector),
	findall(triple(Selector, Predicate, Object),
			rdf(Selector, Predicate, Object),
			SelectorTriples),
	append(TargetTriples, SelectorTriples, Triples).
get_target(_Annotation, []).

assert_tiple(Graph, triple(Subject, Predicate, Object)) :-
	rdf_assert(Subject, Predicate, Object, Graph).

%%	agreeable_annotations(+Type, ConceptScheme)
%
%	Agree on an agreeable set of annotations
% agreeable_annotations(conceptScheme,'http://purl.org/vocab/nl/ubvu/BiblePageConceptScheme').
% agreeable_annotations(domain,'http://accurator.nl/fashion/jewelry#domain').
agreeable_annotations(conceptScheme, ConceptScheme) :-
	% get annotations based on conceptscheme
	 annotations(concept_scheme, ConceptScheme, Annotations),
	 length(Annotations, Number),
	 atomic_list_concat([ConceptScheme, 'Reviews'], Graph),
	 format('~p Annotations are being agreed upon', [Number]),
	 reviews(agree,'http://accurator.nl/user#rasvaan', Annotations, Graph).

agreeable_annotations(domain, Domain) :-
	% get annotations in a specified domain
	 annotations(domain, Domain, Annotations),
	 length(Annotations, Number),
	 atomic_list_concat([Domain, 'Reviews'], Graph),
	 format('~p Annotations are being agreed upon, added to ~p', [Number, Graph]),
	 reviews(agree,'http://accurator.nl/user#rasvaan', Annotations, Graph).
