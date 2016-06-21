:- module(statistics, [annotation_statistics/2]).

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
domain_statistics(_Domain, _{annotations:10, reviewed:5}).
