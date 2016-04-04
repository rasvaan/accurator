/*******************************************************************************
Accurator Item

This code allows the item page to be setup according to the locale, domain
and user settings. The main functionallity regards annoting the item, for which
it relies upon the following files:

- in web/js/components

	* field.js - field objects allowing users to add annotations

	* annotations.js - list of annotations

	* deniche-plugin.js - plugin for annotorious embedding field objects in the
editor div and manages the adding and removal of annotations in the annotation
list

 - in web/js/lib

	* annotorious.min.js - annotorious editor used for annotating fragments of images

*******************************************************************************/
"use strict";

var page = {
	showMetadata: true,
	showAnnotations: true,
	imageId: null, // Set on init image
	fragmentFieldsId: "itemDivAnnotoriousFields", // Id container containing fields
	wholeFieldsId: "itemDivFields"
}

function itemInit() {
	var locale = getLocale();
	var domain = getDomain();
	var uri = getParameterByName("uri");

	populateFlags(locale);

	userLoggedIn()
	.then(function(userData) {
		// user is logged in, so draw page
		drawPage(userData);
	}, function() {
		// user is not logged in, show modal
		var onDismissal = function() {
			document.location.href = "intro.html"
		};

		login(drawPage, onDismissal);
	});

	function drawPage(userData) {
		var ui, annotation_ui;
		var user = userData.user;
		var userName = getUserName(userData.user);

		setLinkLogo("profile");
		populateNavbar(userName, [{link:"profile.html", name:"Profile"}], locale);

		domainSettings(domain)
		.then(function(domainData) {
			ui = domainData.ui + "item";
			annotation_ui = domainData.annotation_ui;
			return getLabels(locale, ui);
		})
		.then(function(labelData){
			var labels = initLabels(labelData);

			// Only show path when cluster is available
			if ((localStorage.getItem("uris") !== null) &&
				(localStorage.getItem("uris") !== "undefined")) {
				addNavigation(uri);
			}

			addButtonEvents(user);
			return events(user, locale, labels);
		})
		.then(function() {
			return setImage(uri);
		})
		.then(function(metadata) {
			displayMetadata(uri);
			displayAnnotations(uri);
			return addAnnotationFields(metadata, user, uri, locale, domain, annotation_ui);
		});
	}
}

function setImage(uri) {
	return $.getJSON("metadata", {uri:uri})
	.then(function(metadata){
		// set id image
		page.imageId = "itemImg" + generateIdFromUri(uri);
		$(".itemImg").attr("id", page.imageId);
		// return info for anotorious
		return metadata;
	});
}

function initLabels(labelData) {
	document.title = labelData.title;
	$("#itemBtnPrevious").append(labelData.itemBtnPrevious);
	$("#itemBtnNext").prepend(labelData.itemBtnNext);
	$("#navbarBtnRecommend").append(labelData.navbarBtnRecommend);
	$("#navbarBtnSearch").append(labelData.navbarBtnSearch);

	var labels = {
		itemHdrFirst: labelData.itemHdrFirst,
		itemTxtFirst: labelData.itemTxtFirst
	};

	return labels;
}

function events(user, locale, labels) {
	return $.getJSON("annotations", {uri:user, type:"user"})
	.then(function(annotations) {
		if(annotations.length === 0) {
			alertMessage(labels.itemHdrFirst, labels.itemTxtFirst, 'success');
		}

		if(annotations.length === 5) {
			var formPersonal = new Form(
				"formPersonal",
				["country", "language", "education", "gender", "birthDate"],
				locale
			);

			formPersonal.addText().then(function() {
				$("#eventsDiv").prepend(formPersonal.node);
			});
		}

		if(annotations.length === 10) {
			var formInternet = new Form(
				"formInternet",
				["socialNetwork", "taggingSites", "taggingExperience", "mail", "mailCheck"],
				locale
			);

			formInternet.addText().then(function() {
				$("#eventsDiv").prepend(formInternet.node);
			});
		}
	});
}

function addNavigation(uri) {
	var uris =  JSON.parse(localStorage.getItem("uris"));
	var pathArray =  JSON.parse(localStorage.getItem("path"));
	var path = new Path(pathArray, "itemDivPath");

	path.enrich()
	.then(function() {
		$("#itemDivPath").append(path.node);
		addNavigationButtonEvents(uris, uri);
	});
}

function addButtonEvents(user) {
	$("#navbarBtnRecommend").click(function() {
		document.location.href = "results.html?user=" + user;
	});
	// Search on pressing enter
	$("#navbarInpSearch").keypress(function(event) {
		if (event.which == 13) {
			var query = encodeURIComponent($("#navbarInpSearch").val());
			document.location.href = "results.html?query=" + query;
		}
	});
	$("#navbarBtnSearch").click(function() {
		var query = encodeURIComponent($("#frmSearch").val());
		document.location.href = "results.html?query=" + query;
	});
}

function addNavigationButtonEvents(uris, uri) {
	var index = uris.indexOf(uri);

	if (index === 0) {
		$("#itemBtnPrevious").attr("disabled", "disabled");
	} else {
		$("#itemBtnPrevious").click(function() {
			document.location.href = "item.html?uri=" + uris[index -1];
		});
	}

	if(index === uris.length-1) {
		$("#itemBtnNext").attr("disabled", "disabled");
	} else {
		$("#itemBtnNext").click(function() {
			document.location.href = "item.html?uri=" + uris[index + 1];
		});
	}
}

function addAnnotationFields(metadata, user, uri, locale, domain, annotation_ui) {
	// Retrieve the fields that should be added (based on save_user_info)
	return $.getJSON("annotation_fields", {
		locale:locale,
		domain:domain,
		annotation_ui:annotation_ui
	})
	.then(function(fields) {
		// console.log(fields);
		// Add fields whole image
		for (var i = 0; i < fields.whole_fields.length; i++) {
			// Create new field object
			var wholeField = new Field(
				fields.whole_fields[i],
				{	id: "whole" + generateIdFromUri(fields.whole_fields[i].uri),
					fragment: false,
					target: uri,
				 	targetImage: metadata.image_uri,
					user: user,
					locale: locale,
			 	 	imageId: page.imageId,
					fieldsId: page.wholeFieldsId
			 	}
			);
			$("#" + wholeField.fieldsId).append(wholeField.node);
		}

		// Add hidden container for fields if there are fragment fields
		if (fields.fragment_fields.length > 0) {
			$(".itemDivHidden").append($.el.div({'id':page.fragmentFieldsId}));
			// Set fields attribute image for annotorious deniche
			$("#" + page.imageId).attr("fields", page.fragmentFieldsId);
		}

		// Add fields to hidden dom elements for annotorious
		for (var i = 0; i < fields.fragment_fields.length; i++) {
			// Create new field object
			var fragmentField = new Field(
				fields.fragment_fields[i],
				{	id: "fragment" + generateIdFromUri(fields.fragment_fields[i].uri),
					fragment: true,
					target: uri,
				 	targetImage: metadata.image_uri,
					user: user,
					locale: locale,
			 	 	imageId: page.imageId,
					fieldsId: page.fragmentFieldsId
			 	}
			);
			// Append the field to div which will be embedded in annotorious
			$("#" + fragmentField.fieldsId).append(fragmentField.node);
		}
		// Add the deniche plugin, which embeds the fields in annotorious
		anno.addPlugin("DenichePlugin", {});
	});
}

function displayMetadata(uri) {
	if(page.showMetadata){
		// Get metadata from server
		$.getJSON("metadata", {uri:uri})
		.done(function(metadata){
			appendMetadataWell(metadata);
		});
	}
}

function appendMetadataWell(metadata) {
	$("#itemDivMetadata").append(
		$.el.div({'class':'row'},
			$.el.div({'class':'col-md-10 col-md-offset-1'},
				$.el.div({'class':'well well-sm'},
				  $.el.h4('Showing metadata for ' + metadata.title),
					$.el.dl({'class':'dl-horizontal',
							 'id':'itemLstMetadata'})))));

	for(var i = 0; i < metadata.properties.length; i++) {
		var encodedQuery = encodeURIComponent(metadata.properties[i].object_label);

		$("#itemLstMetadata").append(
			$.el.dt(metadata.properties[i].predicate_label));
		$("#itemLstMetadata").append(
			$.el.dd(
				$.el.a({'class':'r_undef',
					    'href':'results.html?query=' + encodedQuery},
						metadata.properties[i].object_label)));
	}
}

function displayAnnotations(uri) {
	// Get annotations from server for projecting in well
	if(page.showAnnotations){
		$.getJSON("annotations", {uri:uri, type:"object"})
		.done(function(annotations){
			if(annotations.annotations.length > 0){
				$("#itemDivMetadata").append(annotationWell(annotations));
			}
		});
	}
}

function annotationWell(annotations) {
	$("#itemDivMetadata").append(
		$.el.div({'class':'row'},
			$.el.div({'class':'col-md-10 col-md-offset-1'},
				$.el.div({'class':'well well-sm'},
				  $.el.h4('Annotations for ' + annotations.title),
					$.el.dl({'class':'dl-horizontal',
							 'id':'itemLstAnnotations'})))));


	for(var i = 0; i < annotations.annotations.length; i++) {
		var encodedQuery = encodeURIComponent(annotations.annotations[i].body);

		$("#itemLstAnnotations").append(
			$.el.dt(annotations.annotations[i].field));
		$("#itemLstAnnotations").append(
			$.el.dd(
				$.el.a({'class':'r_undef',
					    'href':'results.html?query=' + encodedQuery},
						annotations.annotations[i].body)));
	}
}
