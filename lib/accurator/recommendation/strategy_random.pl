:- module(strategy_random, [strategy_random/2]).

:- use_module(library(semweb/rdf_db)).

%%      strategy_random(-Result, +Options)
%
%       Assign a number of objects in a random fassion.
strategy_random(Result, Options) :-
	option(target(Target), Options),
    findall(Uri, rdf(Uri, rdf:type, Target), SourceList),
    assign_random(50, SourceList, Result).


assign_random(0, _SourceList, []).
assign_random(_Number, [], []).
assign_random(Number, SourceList, [Uri|List]) :-
    random_member(Uri, SourceList),
    delete(SourceList, Uri, NewSourceList),
    NewNumber is Number-1,
    assign_random(NewNumber, NewSourceList, List).
