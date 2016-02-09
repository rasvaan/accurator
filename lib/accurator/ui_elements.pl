:- module(ui_elements, [
			  get_title/2,
			  uri_label/2,
			  get_elements/3,
			  metadata/3
		  ]).

/** <module> Accurator UI elements
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(http/http_open)).
:- use_module(library(http/url_cache)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_ssl_plugin)).

%%	get_title(+Uri, -Title)
%
%	Returns a title or the last part of the URI if no title is found.
get_title(Uri, Title) :-
    rdf(Uri, dc:title, literal(lang(_,Title))), !.
get_title(Uri, UriLabel) :-
	 iri_xml_namespace(Uri, _, UriLabel), !.
get_title(Uri, Uri).

%%	uri_label(Uri, Label)
%
%	Get the label of an Uri
uri_label(Uri, Label) :-
    rdf_display_label(Uri, _, Label).

%%	get_elements(+Type, -Dic, +Options)
%
%	Determine which type of UI elements to query for.
get_elements(labels, Dic, Options) :-
	get_labels(Dic, Options).
get_elements(countries, Dic, Options) :-
	get_countries(Dic, Options).
get_elements(languages, Dic, Options) :-
	get_languages(Dic, Options).

%%	get_countries(+DictArray, _Options)
%
%	Get a list of country names based on the geonames dataset.
get_countries(DictArray, _Options) :-
	findall(CountryDict,
			(	rdf(GeonamesCountry, gn:featureClass, gn:'A'),
				%only english for now
				rdf(GeonamesCountry, gn:name, literal(lang(en, CountryName))),
				CountryDict0 = country{},
				CountryDict1 = CountryDict0.put(name, CountryName),
				rdf(GeonamesCountry, gn:countryCode, literal(CountryCode)),
				downcase_atom(CountryCode, CountryCodeLower),
				CountryDict = CountryDict1.put(country_code, CountryCodeLower)),
			DictArray).

%%	get_languages(-DictArray, _Options)
%
%	Get a list of languages.
get_languages(DictArray, _Options) :-
	findall(LanguageDict,
			(	rdf(GeonamesCountry, acl:isoCode, literal(IsoCode)),
				LanguageDict0 = language{},
				LanguageDict1 = LanguageDict0.put(iso_code, IsoCode),
				%only english for now
				rdf(GeonamesCountry, rdfs:label, literal(lang(en, LanguageName))),
				LanguageDict = LanguageDict1.put(name, LanguageName)),
			DictArray).

%%	get_labels(-TextDic, +Options)
%
%	Retrieves text elements according to the ui and locale specified in
%	Options.
get_labels(TextDic, Options) :-
	option(locale(Locale), Options),
	option(ui(UI), Options),
	% get all the predicates off this and possible super ui
	setof(Predicate, UI^ui_predicate(UI, Predicate), Predicates),
	maplist(get_text_element(UI, Locale), Predicates, LabelList0),
	get_selector_options(UI, Locale, SelectorFields),
	append(LabelList0, SelectorFields, LabelList),
	dict_pairs(TextDic, elements, LabelList).

ui_predicate(UI, Predicate) :-
	rdf(UI, Predicate, _Object),
	rdf_reachable(Predicate, rdfs:subPropertyOf, auis:uiLabel).
ui_predicate(UI, Predicate) :-
	rdf(UI, rdfs:subClassOf, SuperUI),
	rdf_reachable(Predicate, rdfs:subPropertyOf, auis:uiLabel),
	rdf(SuperUI, Predicate, _Object).

%%	get_text_element(-Label, -Literal, +UI, +Locale)
%
%	Retrieves text elements according to the ui and locale specified in
%	Options. If the ui is not found for the specified UI, the generic
%	super class will be queried.
get_text_element(UI, Locale, Predicate, Label-Literal) :-
	rdf(UI, Predicate, literal(lang(Locale, Literal))),
	rdf(Predicate, rdfs:subPropertyOf, auis:uiLabel),
	!,
	iri_xml_namespace(Predicate, _, Label).

get_text_element(UI, Locale, Predicate, Label-Literal) :-
	rdf(UI, rdfs:subClassOf, SuperUI),
	rdf(SuperUI, Predicate, literal(lang(Locale, Literal))),
	rdf(Predicate,rdfs:subPropertyOf, auis:uiLabel),
	iri_xml_namespace(Predicate, _, Label).

%%	get_selector_options(UI, Locale, SelectorFields)
%
%	Get a list of selector fields based on the specified UI and locale.
get_selector_options(UI, Locale, SelectorFields) :-
	findall(SelectLabel-LiteralArray,
			%HACK: for now only the generic UI has selectfields.
			(	get_selector(UI, Select, SelectLabel),
				get_selector_labels(Select, Locale, LiteralArray)
			),
			SelectorFields).

get_selector(UI, Select, SelectLabel) :-
	% See if current UI has super ui (since only SuperUI has selectors).
	rdf(UI, rdfs:subClassOf, SuperUI),
	!,
	rdf(SuperUI, auis:hasSelect, Select),
	rdf(Select, rdf:type, auis:'SelectField'),
	iri_xml_namespace(Select, _, SelectLabel).
get_selector(UI, Select, SelectLabel) :-
	rdf(UI, auis:hasSelect, Select),
	rdf(Select, rdf:type, auis:'SelectField'),
	iri_xml_namespace(Select, _, SelectLabel).

get_selector_labels(Selector, Locale, LiteralDict) :-
	findall(OptionLabel-LabelDict,
			(	rdf(Selector, auis:hasSelectOption, SelectOption),
				rdf(SelectOption, rdf:type, auis:'SelectOption'),
				rdf(SelectOption, skos:prefLabel, literal(lang(Locale, Literal))),
				rdf(SelectOption, skos:notation, literal(Id)),
				iri_xml_namespace(SelectOption, _, OptionLabel),
				dict_pairs(LabelDict, elements, [label-Literal, id-Id])),
			LiteralArray),
	dict_pairs(LiteralDict, elements, LiteralArray).

%%	metadata(+Type, +Uri, -Metadata)
%
%	Get all properties and subjects attached to a Uri, or get them for a
%	thumbnail.
metadata(full, Uri, Metadata) :-
    findall(property_pair{
				predicate_label:PredicateLabel,
				object_label:ObjectLabel},
	    (	rdf(Uri, Predicate, Object),
			rdf_display_label(Predicate, _, PredicateLabel),
			rdf_display_label(Object, _, ObjectLabel)
	    ),
	    Properties),
	get_title(Uri, DisplayTitle),
	image_url(Uri, ImageLink),
	image_uri(Uri, ImageUri),
	Metadata = metadata{title:DisplayTitle,
						image:ImageLink,
						image_uri:ImageUri,
						properties:Properties}.
metadata(thumbnail, Uri, EnrichedItem) :-
    thumbnail_url(Uri, ThumbnailUrl),
    get_title(Uri, Title),
    EnrichedItem = _{uri:Uri,thumb:ThumbnailUrl,title:Title}.


%%	image_url(+Uri, -ImageUrl) is det.
%
%	* Check if image is present, if so, return request url.
%	* If the image is not present, check if present at url.
%	isShownBy triple.
%	* If not present in cache, database and at
%	server, send image stub.
image_url(Uri, ImageUrl) :-
	check_cache_shown(Uri, Image), !,
    image_link(Image, ImageUrl).
image_url(Uri, ImageUrl) :-
	check_server_shown(Uri, Image), !,
    image_link(Image, ImageUrl).
image_url(Uri, ImageUrl) :-
	check_cache_view(Uri, Image), !,
    image_link(Image, ImageUrl).
image_url(Uri, ImageUrl) :-
	check_server_view(Uri, Image), !,
    image_link(Image, ImageUrl).
image_url(_, Stub) :- http_absolute_location(img('stub_vertical.png'), Stub, []).

check_cache_shown(Uri, Image) :-
	rdf(Aggregation, edm:aggregatedCHO, Uri),
	rdf(Aggregation, edm:isShownBy, Image),
	url_cached(Image, file(_)).
check_cache_view(Uri, Image) :-
	rdf(Aggregation, edm:aggregatedCHO, Uri),
	rdf(Aggregation, edm:hasView, Image0),
	check_if_local(Image0, Image),
	url_cached(Image, file(_)).
check_server_shown(Uri, Image) :-
	rdf(Aggregation, edm:aggregatedCHO, Uri),
    rdf(Aggregation, edm:isShownBy, Image),
    https_header_response(Image, Status),
    Status == 200.
check_server_view(Uri, Image) :-
	rdf(Aggregation, edm:aggregatedCHO, Uri),
    rdf(Aggregation, edm:hasView, Image0),
	check_if_local(Image0, Image),
    https_header_response(Image, Status),
    Status == 200.
check_if_local(Image, ImageUrl) :-
	not(concat('http', _, Image)),
	http_absolute_uri(img(Image), ImageUrl).
check_if_local(ImageUrl, ImageUrl).

image_link(Image, ThumbUrl) :-
	concat('cache/original?uri=', Image, RequestUrl),
    http_absolute_location(root(RequestUrl), ThumbUrl, []).

%%	image_uri(+Uri, -ImageUri)
%
%	Get the 'original' uri of an image, not a link to a cached one.
image_uri(Uri, ImageUri) :-
	rdf(Aggregation, edm:aggregatedCHO, Uri),
	rdf(Aggregation, edm:isShownBy, ImageUri), !.
image_uri(Uri, ImageUri) :-
	rdf(Aggregation, edm:aggregatedCHO, Uri),
	rdf(Aggregation, edm:hasView, ImageUri), !.

%%	thumbnail_url(+Uri, -ThumbUrl)
%
%	* Check if image is present, if so, return request url.
%	* If the image is not present, check if present at url.
%	* If not present in cache, database and at url, send image stub.
thumbnail_url(Uri, ThumbUrl) :-
    check_cache_shown(Uri, Image), !,
	thumb_link(Image, ThumbUrl).
thumbnail_url(Uri, ThumbUrl) :-
    catch(check_server_shown(Uri, Image), _, fail), !,
	thumb_link(Image, ThumbUrl).
thumbnail_url(Uri, ThumbUrl) :-
    check_cache_view(Uri, Image), !,
	thumb_link(Image, ThumbUrl).
thumbnail_url(Uri, ThumbUrl) :-
    catch(check_server_view(Uri, Image), _, fail), !,
	thumb_link(Image, ThumbUrl).
thumbnail_url(_, Stub) :- http_absolute_location(img('stub.png'), Stub, []).

thumb_link(Image, ThumbUrl) :-
	concat('cache/fit?uri=', Image, RequestUrl),
    http_absolute_location(root(RequestUrl), ThumbUrl, []).

%%	https_header_response(+URL, -Status)
%
%	Return the response header of input URL.
https_header_response(URL, Status) :-
    http_open(URL, In,
	  [method(head),
	   status_code(Status),
	   cert_verify_hook(ssl_verify)
	  ]),
    close(In).

:- public ssl_verify/5.

%%	ssl_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
%
%	Currently we accept  all  certificates.
ssl_verify(_SSL,
	   _ProblemCertificate, _AllCertificates, _FirstCertificate,
	   _Error).
