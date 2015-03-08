:- module(expertise, [get_expertise_topics/2,
					  get_domain_topics/2,
					  assert_expertise_relationship/2,
					  get_user_expertise/3,
					  get_user_expertise_domain/2,
					  get_latest_user_expertise/3]).

/** <module> Expertise
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(pairs)).

:- rdf_register_prefix(as, 'http://accurator.nl/schema#').
:- rdf_register_prefix(txn, 'http://lod.taxonconcept.org/ontology/txn.owl#').
:- rdf_register_prefix(hoonoh, 'http://hoonoh.com/ontology#').

%%	get_expertise_topics(-Topics, +Options)
%
%	Retrieves a list of expertise topics, starting from the top concept.
get_expertise_topics(Topics, Options) :-
	option(locale(Locale), Options),
	option(topConcept(TopConcept), Options),
	option(numberOfTopics(Number), Options),
	get_number_topics([TopConcept], Number, TopicUris),
	maplist(get_info_topics(Locale, Options), TopicUris, TopicDicts),
	Topics = expertise_topics{topics:TopicDicts}.

%%	get_domain_topics(+Domain, -Topics)
%
%	Retrieves a list of expertise topics, starting from the top concept.
get_domain_topics(Domain, Topics) :-
	rdf(DomainUri, rdf:type, as:'Domain'),
	rdf(DomainUri, rdfs:label, literal(Domain)),
	rdf(DomainUri, skos:hasTopConcept, TopConcept),
	rdf(DomainUri, as:hasMaximumExpertiseTopics, literal(NumberLiteral)),
	atom_number(NumberLiteral, Number),
	get_number_topics([TopConcept], Number, Topics).


%%	get_number_topics(Concepts, Number, PreviousTopics, PreviousTopics)
%
%	Find a list of topics, smaller than the specified number. If the new
%	number exceeds the specified number, the previous list of concepts
%	is returned.
get_number_topics(Concepts, Number, Topics) :-
	maplist(get_children, Concepts, ChildrenLists),
	append(ChildrenLists, Children),
	length(Children, NumberChildren),
	NumberChildren < Number,
	get_number_topics(Children, Number, Topics).
get_number_topics(Topics, _Number, Topics) :-
	!.

%%	get_children(+Concept, -ChildrenList)
%
%	Get a list of children for the concept.
get_children(Concept, ChildrenList) :-
	findall(Child,
			get_narrow_child(Concept, Child),
			ChildrenList),
	not(length(ChildrenList, 0)), !.

get_children(Concept, ChildrenList) :-
	findall(Child,
			get_broader_child(Concept, Child),
			ChildrenList),
	not(length(ChildrenList, 0)), !.

get_children(_Concept, []).

get_narrow_child(Concept, Child) :-
	rdf_has(Concept, skos:narrower, Child),
	rdf(Child, rdf:type, skos:'Concept').

get_broader_child(Concept, Child) :-
	rdf_has(Child, skos:broader, Concept),
	rdf(Child, rdf:type, skos:'Concept').

%add subproperty query

%%	get_info_topics(+Locale, +Options, +Uri, -Dict)
%
%	Get label of topics and when specified the children of topics.
get_info_topics(Locale, Options, Uri, Dict) :-
	rdf_global_id(Uri, GlobalUri),
	get_label(Locale, Uri, Label),
	option(numberOfChildren(Number), Options),
	get_childrens_labels(Uri, Locale, Number, ChildrensLabels),
	Dict = topic{uri:GlobalUri, label:Label, childrens_labels:ChildrensLabels}.

%%	get_childrens_labels(+Uri, +Locale, +MaxNumber, -Labels)
%
%	Retrieve the labels of child nodes.
get_childrens_labels(_Uri, _Locale, 0, []) :- !.
get_childrens_labels(Uri, Locale, MaxNumber, Labels) :-
	get_children(Uri, Children),
	maplist(get_label(Locale), Children, LongLabels),
	shorten_when_needed(LongLabels, MaxNumber, Labels).

shorten_when_needed(LongLabels, MaxNumber, LongLabels) :-
	length(LongLabels, Length),
	Length =< MaxNumber, !.

shorten_when_needed(LongLabels, MaxNumber, Labels) :-
	append(Labels, _Rest, LongLabels),
	length(Labels, MaxNumber).

%%	assert_expertise_relationship(+User, +TopicValuePair)
%
%	Save a topic value pair. First the predicates and objects linked to
%	this expertise are created. This list is sorted and used to create a
%	hash with. Triples are asserted linking to the resulting hash based
%	uri.
assert_expertise_relationship(User, Topic-Value) :-
	get_time(Time),
	format_time(atom(TimeStamp), '%FT%T%:z', Time),
    % Create a sortable list of thing to assert
	KeyValue0 = [
	    po(rdf:type, hoonoh:'ExpertiseRelationship'),
	    po(as:createdAt, literal(type(xsd:dateTime, TimeStamp))),
	    po(hoonoh:from, User),
	    po(hoonoh:value, literal(type(xsd:decimal, Value))),
	    po(hoonoh:toTopic, Topic)
	],
	sort(KeyValue0, KeyValue),
	rdf_global_term(KeyValue, Pairs),
	variant_sha1(Pairs, Hash),
	hash_uri(Hash, Expertise),
	maplist(po2rdf(Expertise), Pairs, Triples),
		rdf_transaction(
	     (	 forall(member(rdf(S,P,O), Triples),
			rdf_assert(S,P,O, User)))).

hash_uri(Hash, Uri) :-
	nonvar(Hash), Hash \= null,
	!,
	atomic_list_concat(['http://accurator.nl/expertise#', Hash], Uri).

po2rdf(S,po(P,O),rdf(S,P,O)).

%%  get_user_expertise(+User, ?Topic, -TopicDateValueDictList)
%
%	Get all values and corresponding dates based on user and topic.
get_user_expertise(User, Topic, Topic-DateValueDictList) :-
	findall(DateValueDict,
			(	rdf(Expertise, hoonoh:from, User),
				rdf(Expertise, hoonoh:toTopic, Topic),
				rdf(Expertise, hoonoh:value, literal(type(xsd:decimal, Value))),
				rdf(Expertise, accu:createdAt, literal(type(xsd:dateTime, Date))),
				dict_pairs(DateValueDict, elements, [value-Value, date-Date])),
			DateValueDictList).

%%  get_latest_user_expertise(+User, +Topic, -TopicValuePair)
%
%	Get latest value based on user and topic.
get_latest_user_expertise(User, Topic, Topic-Value) :-
	findall(Date-DatedValue,
	    ( rdf(Expertise, hoonoh:from, User),
		  rdf(Expertise, hoonoh:toTopic, Topic),
		  rdf(Expertise, hoonoh:value, literal(type(xsd:decimal, DatedValue))),
		  rdf(Expertise, accu:createdAt, literal(type(xsd:dateTime, Date)))),
			DateValuePairs),
	keysort(DateValuePairs, SortedPairs),
	reverse(SortedPairs, ReversePairs),
	member(Date-Value, ReversePairs).

%%	get_label(Locale, Uri, Label)
%
%	Get a label for a specified uri and locale. Three different
%	attempts:
%
%	* with locale specified
%	* ignoring the type of specified locale
%	* literals without locale specified
%
%	Three different label properties are tried:
%
%	* txn:commonName - a life sciences specific label
%	* skos:prefLabel - skos
%	* subproperties of skos:prefLabel
get_label(Locale, Uri, Label) :-
	rdf(Uri, txn:commonName, literal(lang(Locale, Label))), !.
get_label(Locale, Uri, Label) :-
	rdf(Uri, skos:prefLabel, literal(lang(Locale, Label))), !.
get_label(Locale, Uri, Label) :-
	rdf_has(Uri, skos:prefLabel, literal(lang(Locale, Label))), !.
get_label(_Locale, Uri, Label) :-
	rdf(Uri, txn:commonName, literal(lang(_, Label))), !.
get_label(_Locale, Uri, Label) :-
	rdf(Uri, skos:prefLabel, literal(lang(_, Label))), !.
get_label(_Locale, Uri, Label) :-
	rdf_has(Uri, skos:prefLabel, literal(lang(_, Label))), !.
get_label(_Locale, Uri, Label) :-
	rdf(Uri, txn:commonName, literal(Label)), !.
get_label(_Locale, Uri, Label) :-
	rdf(Uri, skos:prefLabel, literal(Label)), !.
get_label(_Locale, Uri, Label) :-
	rdf_has(Uri, skos:prefLabel, literal(Label)), !.

%%	get_user_expertise_domain(-ExpertiseValues, +Options)
%
%	Retrieves a list of expertise values based on user and domain.
get_user_expertise_domain(ExpertiseValues, Options) :-
	option(user(User), Options),
	option(domain(Domain), Options),
	get_domain_topics(Domain, Topics),
	maplist(get_latest_user_expertise(User), Topics, ExpertiseValues).

