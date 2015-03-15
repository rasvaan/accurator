:- module(strategy_expertise, [strategy_expertise/2]).

:- use_module(library(accurator/accurator_user)).
:- use_module(library(accurator/expertise)).
:- use_module(library(cluster_search/cs_filter)).
:- use_module(library(cluster_search/rdf_search)).
:- use_module(library(cluster_search/owl_ultra_lite)).
:- use_module(api(cluster_search)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_db)).


:- multifile
	cluster_search:predicate_weight/2.

%strategy_expertise(Result, []).

%%      strategy_expertise(-Result, +Options)
%
%       Assign a number of objects in a random fassion.
strategy_expertise(Clusters, Options0) :-
	option(user(User), Options0),
	get_domain(User, Domain),
	Options = [domain(Domain) | Options0],
	set_expertise_agenda(3, Agenda, Options),
    cluster_recommender(Agenda, State, Options),
	OrganizeOptions = [groupBy(path)],
    organize_resources(State, Clusters, OrganizeOptions).

%%	set_expertise_agenda(+MaxNumber, -Agenda, +Options)
%
%	Set the agenda by retrieving all the expertise values of user given
%	a domain, sort based on the values and pick the highest values with
%	a maximum number.
set_expertise_agenda(MaxNumber, Agenda, Options) :-
	option(user(User), Options),
	option(domain(Domain), Options),
	get_user_expertise_domain(User, Domain, ExpertiseValues),
	transpose_pairs(ExpertiseValues, SortedExpertiseValues),
	%determine the number of expertise levels to be picked
	length(SortedExpertiseValues, NumberExpertise),
	number_of_items(NumberExpertise, MaxNumber, NumberItems),
	group_pairs_by_key(SortedExpertiseValues, ReverseGroupedValues),
	reverse(ReverseGroupedValues, GroupedValues),
	expertise_from_bins(GroupedValues, 0, NumberItems, Agenda).

%%	expertise_from_bins(+Bins, +Counter,+ MaxN, -Agenda)
%
%	Determine the agenda by picking expertise values from bins. When a
%	bin has multiple expertise areas, pick one at random from that bin.
expertise_from_bins(_Bins, MaxN, MaxN, []).
expertise_from_bins(Bins, N0, MaxN, [Expertise|List]) :-
	N is N0 + 1,
	random_from_bin(Bins, NewBins, Expertise),
	expertise_from_bins(NewBins, N, MaxN, List).

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

%%	number_of_items(+NumberExpertise, +Number0, -NumberExpertise)
%
%	Determine the maximum number of agenda items.
number_of_items(NumberExpertise, Number0, NumberExpertise) :-
	NumberExpertise < Number0, !.
number_of_items(_NumberExpertise, Number0, Number0).

cluster_recommender(Agenda, State, Options) :-
	option(target(Target), Options),
	filter_to_goal([type(Target)], R, Goal, Options),
	%unbound steps for now
	Steps = -1,
	%constructed search goal
	TargetCond = target_goal(Goal, R),
	%define edges used
	Expand = rdf_backward_search:edge,
	%add that to options, set threshold low to enable graph traphersal
	SearchOptions = [edge_limit(30), threshold(0.0001),
					 expand_node(Expand), graphOutput(spo)],
	%init search state
	rdf_init_state(TargetCond, State, SearchOptions),
	%start the search with set agenda (instead of keyword)
	rdf_start_search(Agenda, State),
	%do the steps
	steps(0, Steps, State),
	%prune the graph
	prune(State, []).

steps(Steps, Steps, _) :- !.
steps(I, Steps, Graph) :-
	I2 is I + 1,
	(   rdf_extend_search(Graph)
	->  (   debugging(rdf_search)
	    ->  debug(rdf_search, 'After cycle ~D', [I2]),
		forall(debug_property(P),
		       (   rdf_search_property(Graph, P),
			   debug(rdf_search, '\t~p', [P])))
	    ;   true
	    ),
	    steps(I2, Steps, Graph)
	;   debug(rdf_search, 'Agenda is empty after ~D steps~n', [I])
	).

debug_property(target_count(_)).
debug_property(graph_size(_)).

prune(State, Options) :-
    rdf_prune_search(State, Options),
    rdf_search_property(State, graph_size(Nodes)),
    debug(rdf_search, 'After prune: ~D nodes in graph', [Nodes]).


%%	edge(+Node, +Score, -Link) is nondet.
%
%	Generate links from Object Node, consisting of a Subject,
%	Predicate and Weight.

edge(O, _, i(S,P,W)) :-
	edge(O, S, P, W),
	debug(myedge, 'Expanding ~2f ~p ~p ~p~n', [W, O, P, S]),
	W > 0.0001.

edge(O, S, P, W) :-
	setof(S, i_edge(O, S, P), Ss),
	(   predicate_weight(P, W)
	->  member(S, Ss)
	;   length(Ss, Len),
	    member(S, Ss),
	    subject_weight(S, Len, W)
	).


%%	i_edge(+O, -S, -P)
%
%	Find Subject that is connected through Predicate.

% annotation edge hack: translate the connection between object and
% subject through an annotation into a dc:subject predicate.
i_edge(O, S, P) :-
	rdf(Annotation, oa:hasBody, O),
        rdf(Annotation, oa:hasTarget, S),
	rdf_equal(P, dc:subject).


i_edge(O, S, P) :-
	rdf(S, P, O),
	% ignore annotations connected by hasTarget
	\+ rdf_equal(S, oa:hasTarget).
i_edge(O, S, P) :-
	rdf(O, P0, S),
	atom(S),
	(   owl_ultra_lite:inverse_predicate(P0, P)
	->  true
	;   predicate_weight(P0, 1)
	->  P = P0
	),
	% ignore annotations connected by hasTarget
	\+ rdf_equal(S, oa:hasTarget).

%%	predicate_weight(+Predicate, -Weight) is semidet.
%
%	Weight based on the meaning of   Predicate. This predicate deals
%	with RDF predicates that have a  well defined meaning.
%
%	Additional weights (or overwrites) can be defined in
%	cluster_search:predicate_weight/2,
%
%	Note that rdfs:comment is not searched as  it   is  supposed to
%	be comment about the graph, and not part of the graph itself.

predicate_weight(P, Weight) :-
	catch(cluster_search:predicate_weight(P, Weight), _, fail), !.

predicate_weight(P, 1) :-
	rdfs_subproperty_of(P, rdfs:label), !.
predicate_weight(P, 1) :-
	rdfs_subproperty_of(P, rdf:value), !.
predicate_weight(P, 1) :-
	rdf_equal(P, owl:sameAs), !.
predicate_weight(P, 1) :-
	rdf_equal(P, skos:exactMatch), !.
predicate_weight(P, 0) :-
	rdfs_subproperty_of(P, rdfs:comment), !.

subject_weight(S, _, 1) :-
	rdf_is_bnode(S), !.
subject_weight(_, Count, W) :-
	W is 1/max(3, Count).
