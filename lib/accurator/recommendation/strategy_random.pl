:- module(strategy_random, [strategy_random/2]).

:- use_module(library(accurator/accurator_user)).
:- use_module(library(semweb/rdf_db)).

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
