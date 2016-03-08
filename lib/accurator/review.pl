:- module(review, [
			  review/3,
			  reviews/3]).

:- use_module(library(oa_annotation)).
:- use_module(library(semweb/rdf_db)).

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
	Options = [
		target(Uri),
		label(literal(Judgement)),
		user(User),
		motivation('http://www.w3.org/ns/oa#moderating')
	],
	debug(review, 'Add review: ~p', [Options]).
	%rdf_add_annotation(Options, _Annotation).
