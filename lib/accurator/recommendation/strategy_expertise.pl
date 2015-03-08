:- module(strategy_expertise, [strategy_expertise/2]).

:- use_module(library(accurator/accurator_user)).

%%      strategy_expertise(-Result, +Options)
%
%       Assign a number of objects in a random fassion.
strategy_expertise(_Result, Options) :-
	option(target(_Target), Options),
	option(user(User), Options),
	get_domain(User, Domain),
	option(domain(Domain), Options) .

