:- module(strategy_random, [strategy_random/2,
							strategy_ranked_random/2,
							strategy_user_ranked_random/2,
							random_from_bin/3]).

:- use_module(library(accurator/accurator_user)).
:- use_module(library(accurator/annotation)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(random)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).

%%      strategy_random(-Result, +Options)
%
%       Assign a number of objects in a random fassion.
strategy_random(Result, Options) :-
	option(target(Target), Options),
	option(number(Number), Options),
	option(filter(Filter), Options),
	% Get list of all targets
    findall(Uri, rdf(Uri, rdf:type, Target), SourceList),
	filter(Filter, SourceList, FilteredList, Options),
    assign_random(Number, FilteredList, Result).

%%	filter(+FilterOption, +Uris, -FilteredUris, +Options)
%
%	Filter the potential candidates.
filter(annotated, SourceList, FilteredUris, Options) :-
	option(user(User), Options),
	get_annotated_user(User, AnnotatedUris),
	subtract(SourceList, AnnotatedUris, FilteredUris).
filter(none, SourceList, SourceList, _Options).


assign_random(0, _SourceList, []).
assign_random(_Number, [], []).
assign_random(Number, SourceList, [Uri|List]) :-
    random_member(Uri, SourceList),
    delete(SourceList, Uri, NewSourceList),
    NewNumber is Number-1,
    assign_random(NewNumber, NewSourceList, List).


%%      strategy_user_ranked_random(-Result, +Options)
%
%		Assign a number of objects in a random fassion, prioritysing
%		items with low numbers of users that annotated them.
strategy_user_ranked_random(Result, Options) :-
	option(target(Target), Options),
	option(number(Number), Options),
	option(filter(Filter), Options),
	% get list of all targets
    findall(Uri, rdf(Uri, rdf:type, Target), SourceList),
	filter(Filter, SourceList, FilteredList, Options),
	maplist(number_of_users_pair, FilteredList, PairList),
	% sort to create proper bins
	keysort(PairList, SortedList),
	group_pairs_by_key(SortedList, Bins),
	results_from_bins(Bins, 0, Number, Result).

number_of_users_pair(Uri, Number-Uri) :-
	number_of_users(Uri, Number).


%%      strategy_ranked_random(-Result, +Options)
%
%		Assign a number of objects in a random fassion, prioritysing
%		items with low numbers of annotations.
strategy_ranked_random(Result, Options) :-
	option(target(Target), Options),
	option(number(Number), Options),
	option(filter(Filter), Options),
	% get list of all targets
    findall(Uri, rdf(Uri, rdf:type, Target), SourceList),
	filter(Filter, SourceList, FilteredList, Options),
	maplist(number_of_annotations_pair, FilteredList, PairList),
	% sort to create proper bins
	keysort(PairList, SortedList),
	group_pairs_by_key(SortedList, Bins),
	results_from_bins(Bins, 0, Number, Result).

number_of_annotations_pair(Uri, Number-Uri) :-
	number_of_annotations(Uri, Number).


%%	results_from_bins(+Bins, +Counter, +MaxN, -Agenda)
%
%	Get results by picking uris from bins sorted by the number of
%	annotatoins (inverse would be interesting for reviewing!). When a
%	bin has multiple uris, pick one at random from that bin.
results_from_bins([], _, _, []) :- !.
results_from_bins(_Bins, MaxN, MaxN, []) :- !.
results_from_bins(Bins, N0, MaxN, [Result|List]) :-
	N is N0 + 1,
	random_from_bin(Bins, NewBins, Result),
	results_from_bins(NewBins, N, MaxN, List).

%%	random_from_bin(+Bins, +NewBins, -Expertise)
%
%	Get a random value from the bin. If the bin is empty aftwerwards,
%	remove from bins, otherwise only remove the value from the bin.
random_from_bin([Value-Bin|Bins], NewBins, Expertise) :-
	length(Bin, Number0),
	Number is Number0-1,
	random_between(0, Number, Index),
	nth0(Index, Bin, Expertise, Rest),
	(  Rest == []
	-> NewBins = Bins
	;  NewBins = [Value-Rest|Bins]
	).
