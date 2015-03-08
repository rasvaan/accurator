:- module(strategy_expertise, [strategy_expertise/2]).

:- use_module(library(accurator/accurator_user)).
:- use_module(library(accurator/expertise)).

%%      strategy_expertise(-Result, +Options)
%
%       Assign a number of objects in a random fassion.
strategy_expertise(_Result, _Options) :-
	%option(target(_Target), Options),
	%Remove line below
	User = 'http://accurator.nl/user#kip',
	get_domain(User, DomainString),
	atom_string(Domain, DomainString),
	set_expertise_agenda(5, _Agenda, [user(User), domain(Domain)]).

set_expertise_agenda(MaxNumber, Agenda, Options) :-
	get_user_expertise_domain(ExpertiseValues, Options),
	transpose_pairs(ExpertiseValues, SortedExpertiseValues),
	reverse(SortedExpertiseValues, TransposedExpertiseValues),
	length(TransposedExpertiseValues, NumberExpertise),
	number_of_items(NumberExpertise, MaxNumber, NumberItems),
	length(AgendaPairs, NumberItems),
	append(AgendaPairs, _, TransposedExpertiseValues),
	maplist(pair_single, AgendaPairs, Agenda).

number_of_items(NumberExpertise, Number0, NumberExpertise) :-
	NumberExpertise < Number0, !.
number_of_items(_NumberExpertise, Number0, Number0).

pair_single(_Left-Right, Right).
