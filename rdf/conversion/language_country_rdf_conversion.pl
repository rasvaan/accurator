:- module(country_scrape, [save_countries/0,
						  save_languages/0]).

:- use_module(library(http/json)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle_write)).% Save results

:- rdf_register_prefix(acl, 'http://semanticweb.cs.vu.nl/accurator/languages/').
:- rdf_register_prefix(gn, 'http://www.geonames.org/ontology#').

save_countries :-
	rdf_unload_graph('http://sws.geonames.org/countries/'),
	%http://api.geonames.org/countryInfoJSON?username=touristguide
	open('countries.json', read, Stream, []),
	json_read_dict(Stream, Dict),
	close(Stream),
	Countries = Dict.geonames,
	findall(country(CountryName, GeoNamesID, Languages, Code),
			(	member(Country, Countries),
				CountryName = Country.countryName,
				GeoNamesID = Country.geonameId,
				Languages = Country.languages,
				Code = Country.countryCode),
			CountryNames),
	maplist(add_country_rdf, CountryNames),
	rdf_save_turtle('countries.ttl', [graph('http://sws.geonames.org/countries/')]).

add_country_rdf(country(CountryNameString, GeoNamesID, LanguagesString, CodeString)) :-
	atomic_list_concat(['http://sws.geonames.org/', GeoNamesID, '/'], GeoNamesUri),
	atom_string(CountryName, CountryNameString),
	atom_string(Languages, LanguagesString),
	atom_string(Code, CodeString),
	rdf_assert(GeoNamesUri, gn:geonamesID, literal(GeoNamesID), 'http://sws.geonames.org/countries/'),
	rdf_assert(GeoNamesUri, gn:name, literal(lang(en, CountryName)), 'http://sws.geonames.org/countries/'),
	rdf_assert(GeoNamesUri, gn:featureClass, gn:'A', 'http://sws.geonames.org/countries/'),
	rdf_assert(GeoNamesUri, gn:languages, literal(Languages), 'http://sws.geonames.org/countries/'),
	rdf_assert(GeoNamesUri, gn:countryCode, literal(Code), 'http://sws.geonames.org/countries/'),
	debug(country, 'countries ~p ~p ~p', [CountryName, GeoNamesID, Languages]).

save_languages :-
	rdf_unload_graph('http://semanticweb.cs.vu.nl/accurator/languages/'),
	open('languages.json', read, Stream, []),
	json_read_dict(Stream, LanguagesDict),
	close(Stream),
	dict_pairs(LanguagesDict, elements, Languages),
	findall(language(LanguageName, LanguageNativeName, IsoCode),
			(	member(LanguagePair, Languages),
				get_code_dict(LanguagePair, IsoCodeString, Language),
				atom_string(IsoCode, IsoCodeString),
				LanguageNameString = Language.name,
				atom_string(LanguageName, LanguageNameString),
				LanguageNativeNameString = Language.nativeName,
				atom_string(LanguageNativeName, LanguageNativeNameString)
			),
			LanguageNames),
	maplist(add_language_rdf, LanguageNames),
	rdf_save_turtle('languages.ttl', [graph('http://semanticweb.cs.vu.nl/accurator/languages/')]).

get_code_dict(Code - Dict, Code, Dict).

add_language_rdf(language(LanguageName, LanguageNativeName, IsoCode)) :-
	atomic_list_concat(['http://semanticweb.cs.vu.nl/accurator/languages/', IsoCode, '/'], LanguageUri),
	rdf_assert(LanguageUri, rdfs:label, literal(lang(en, LanguageName)), 'http://semanticweb.cs.vu.nl/accurator/languages/'),
	rdf_assert(LanguageUri, rdfs:label, literal(LanguageNativeName), 'http://semanticweb.cs.vu.nl/accurator/languages/'),
	rdf_assert(LanguageUri, acl:isoCode, literal(IsoCode), 'http://semanticweb.cs.vu.nl/accurator/languages/').
