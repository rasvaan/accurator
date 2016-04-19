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
	findall(DomainUri,
			rdf(DomainUri, rdf:type, accu:'Domain'),
			DomainUris),
	get_root_domains(DomainUris, Domains),
	Dic = Domains.
get_domain_settings(Dic, Options) :-
	option(domain(Domain), Options),
	rdf(DomainUri, rdf:type, accu:'Domain'),
	rdf(DomainUri, rdfs:label, literal(Domain)),
	get_domain_dic(DomainUri, Domain, Dic).

%%	get_root_domains(+Domain, -RootDomains)
%
%	Return a list of domains that do not have a super domain.
get_root_domains([], []) :- !.
get_root_domains([Domain | DomainUris], Filtered) :-
	% skip domain if it has superdomain
	rdf(_SuperDomain, accu:subDomains, Domain), !,
	get_root_domains(DomainUris, Filtered).
get_root_domains([Domain | DomainUris],  [DomainLabel | Filtered]) :-
	rdf(Domain, rdfs:label, literal(DomainLabel)),
	get_root_domains(DomainUris, Filtered).

%%	get_domain_dic(+DomainUri, +Domain, -Dic)
%
%	Create a dictionary filled with information about the domain.
get_domain_dic(DomainUri, Domain, Dic) :-
	setof(Property, Value^rdf(DomainUri, Property, Value), Properties),
	get_values(DomainUri, Properties, PropertyPairs0),
	domain_image(DomainUri, ImagePairs),
	super_domain(DomainUri, PropertyPairs0, PropertyPairs),
	append([[domain-Domain], PropertyPairs, ImagePairs], Pairs),
	dict_pairs(Dic, elements, Pairs).

%%	get_values(+DomainUri, +Properties, -Pairs)
%
%	Generate a list of pairs. The value of the pair is either a list or
%	an atom. The PropertyLabel is abstracted from the property.
get_values(_DomainUri, [], []) :- !.
get_values(DomainUri, [Property|Properties], [PropertyLabel-Values|Pairs]) :-
	findall(Value,
			rdf_value(DomainUri, Property, _Label, Value),
			Values),
	length(Values, Length),
	Length > 1, !,
	iri_xml_namespace(Property, _, PropertyLabel),
	get_values(DomainUri, Properties, Pairs).
get_values(DomainUri, [Property|Properties], [PropertyLabel-Value|Pairs]) :-
	rdf_value(DomainUri, Property, PropertyLabel, Value),
	get_values(DomainUri, Properties, Pairs).


%%	rdf_value(+Uri, -Property, Value)
%
%	Get the label of an RDF property and either the literal or the
%	resource value.
rdf_value(DomainUri, PropertyUri, PropertyLabel, Value) :-
	rdf(DomainUri, PropertyUri, literal(Value)),
	iri_xml_namespace(PropertyUri, _, PropertyLabel).
rdf_value(DomainUri, PropertyUri, PropertyLabel, Value) :-
	rdf(DomainUri, PropertyUri, Value),
	rdf_resource(Value),
	iri_xml_namespace(PropertyUri, _, PropertyLabel).

%%	domain_image(+DomainUri, -ImageList)
%
%	Get info about an image in the form of a list of pairs
domain_image(DomainUri, [image-ImagePath, imageBrightness-Brightness]) :-
	rdf(DomainUri, accu:hasDescriptiveImage, Image), !,
	rdf(Image, accu:hasFilePath, literal(ImagePath)),
	rdf(Image, accu:brightness, literal(Brightness)).
domain_image(_DomainUri, []).

%%	super_domain(+Domain, +Pairs0, -Pairs1)
%
%	Retrieve the super domain of a domain and when possible add to list
%	of pairs.
super_domain(Domain, Pairs, [superDomain-SuperDomain | Pairs]) :-
	rdf(SuperDomain, accu:subDomains, Domain), !.
super_domain(_Domain, Pairs, Pairs).


