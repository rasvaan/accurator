:- module(strategy_expertise, [strategy_expertise/2]).


%%      strategy_expertise(-Result, +Options)
%
%       Assign a number of objects in a random fassion.
strategy_expertise(Result, Options) :-
	option(user(User), Options),
	option(domain(Domain), Options),
	option(target(Target), Options).

