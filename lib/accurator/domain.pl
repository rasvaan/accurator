:- module(domain, [get_domain_settings/2]).

/** <module> Domain
*/
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- rdf_meta
	rdf_value(r, -, -).

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
	findall(Property-Value,
			rdf_value(DomainUri, Property, Value),
			PropertyPairs),
	domain_image(DomainUri, ImagePairs),
	sub_domains(DomainUri, SubDomains),
	append([[domain-Domain], PropertyPairs, ImagePairs, [subDomain-SubDomains]], Pairs),
	dict_pairs(Dic, elements, Pairs).

%%	rdf_value(+Uri, -Property, Value)
%
%	Get the label of an RDF property and either the literal or the
%	resource value.
rdf_value(DomainUri, Property, Value) :-
	rdf(DomainUri, PropertyUri, literal(Value)),
	iri_xml_namespace(PropertyUri, _, Property).
rdf_value(DomainUri, Property, Value) :-
	rdf(DomainUri, PropertyUri, Value),
	rdf_resource(Value),
	iri_xml_namespace(PropertyUri, _, Property).

%%	domain_image(+DomainUri, -ImageList)
%
%	Get info about an image in the form of a list of pairs
domain_image(DomainUri, [image-ImagePath, imageBrightness-Brightness]) :-
	rdf(DomainUri, accu:hasDescriptiveImage, Image), !,
	rdf(Image, accu:hasFilePath, literal(ImagePath)),
	rdf(Image, accu:brightness, literal(Brightness)).
domain_image(_DomainUri, []).

%%	sub_domains(+DomainUri, -SubdomainList)
%
%	Return a list of sub domains
sub_domains(DomainUri, SubDomains) :-
	rdf_has(DomainUri, accu:subDomains, RdfList), !,
	rdfs_list_to_prolog_list(RdfList, SubDomains).
sub_domains(_DomainUri, []).
