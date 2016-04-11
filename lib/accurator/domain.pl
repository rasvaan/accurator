:- module(domain, [get_domain_settings/2,
				   get_topic_settings/2]).

/** <module> Domain
*/
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

%%	get_domain_settings(-Dic, +Options)
%
%	If no domain is provided, return available domains. If domain is
%	the given option and it exists, get dict with values,
%	otherwise return dic with the generic settings
get_domain_settings(Dic, Options) :-
	option(domain(Domain), Options),
	var(Domain), !,
	findall(Domain,
			(	rdf(DomainUri, rdf:type, accu:'Domain'),
				rdf(DomainUri, rdfs:label, literal(Domain))),
			Domains),
	Dic = Domains.

get_domain_settings(Dic, Options) :-
	option(domain(Domain), Options),
	rdf(DomainUri, rdf:type, accu:'Domain'),
	rdf(DomainUri, rdfs:label, literal(Domain)),
	get_domain_dic(DomainUri, Domain, Dic).
get_domain_settings(Dic, _Options) :-
	get_domain_dic('http://accurator.nl/generic#domain', 'generic', Dic).

%%	get_domain_dic(+DomainUri, +Domain, -Dic)
%
%	Create a dictionary filled with information about the domain.
get_domain_dic(DomainUri, Domain, Dic) :-
	rdf(DomainUri, dcterms:requires, Taxonomy),
	rdf(DomainUri, accu:hasTarget, Target),
	rdf(DomainUri, skos:hasTopConcept, TopConcept),
	rdf(DomainUri, accu:hasMaximumExpertiseTopics, literal(MaxTopics)),
	rdf(DomainUri, accu:hasMaximumChildren, literal(MaxChildren)),
	rdf(DomainUri, accu:hasUI, UI),
	rdf(DomainUri, accu:hasAnnotationUI, AnnotationUI),
	rdf(DomainUri, accu:hasDescriptiveImage, Image),
	rdf(Image, accu:hasFilePath, literal(ImagePath)),
	rdf(Image, accu:brightness, literal(Brightness)),
	rdf_has(DomainUri, accu:topics, RdfList), !,
	rdfs_list_to_prolog_list(RdfList, Topics),
	Dic = domain{domain:Domain,
				 target:Target,
				 taxonomy:Taxonomy,
				 top_concept:TopConcept,
				 number_of_topics:MaxTopics,
				 number_of_children_shown:MaxChildren,
				 ui:UI,
				 annotation_ui:AnnotationUI,
				 image:ImagePath,
				 image_brightness:Brightness,
				 topics:Topics}.


%%	get_topic_settings(-Dic, +Options)
%
%	If no topic is provided, return available topics for . If domain is
%	the given option and it exists, get dict with values,
%	otherwise return dic with the generic settings
get_topic_settings(Dic, Options) :-
	option(domain(Domain), Options),
	var(Domain), !,
	rdf(DomainUri, rdf:type, accu:'Domain'),
	rdf(DomainUri, rdfs:label, literal(Domain)),
	findall(Topic,
			rdf(DomainUri, accu:topics, Topic),
			Topics),
	Dic = Topics.
