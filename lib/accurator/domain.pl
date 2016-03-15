:- module(domain, [get_domain_settings/2]).

/** <module> Domain
*/
:- use_module(library(semweb/rdf_db)).

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
	rdf(DomainUri, accu:hasLastImage, Image2),
	rdf(Image2, accu:hasFilePath, literal(ImagePath2)),
	rdf(Image2, accu:brightness, literal(Brightness2)),
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
				 last_image:ImagePath2,
				 last_image_brightness:Brightness2}.
