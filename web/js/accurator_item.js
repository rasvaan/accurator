/*******************************************************************************
Accurator Item
This code allows the item page to be setup according to the locale, domain
and user settings. The main functionallity regards annoting the item, for which
it relies upon the following files:

* field.js - field objects allowing users to add annotations

* annotations.js - list of annotations

* annotorious.min.js - annotorious editor used for annotating fragments of images

* deniche-plugin.js - plugin for annotorious embedding field objects in the
editor div and manages the adding and removal of annotations in the annotation
list

*******************************************************************************/
"use strict";
var query, locale, experiment, domain, user, ui, annotation_ui, uri;
var vntFirstTitle, vntFirstText;

var page = {
	showMetadata: true,
	showAnnotations: true,
	imageId: null, // Set on init image
	fragmentFieldsId: "itemDivAnnotoriousFields", // Id container containing fields
	wholeFieldsId: "itemDivFields"
}

function itemInit() {
	locale = getLocale();
	domain = getDomain();
	experiment = getExperiment();
	uri = getParameterByName("uri");

	populateFlags(locale);

	// Make sure user is logged in
	var onLoggedIn = function(loginData) {
		setLinkLogo("profile");

		// Get domain settings before populating ui
		var onDomain = function(domainData) {
			user = loginData.user;
			var userName = getUserName(loginData.user);
			ui = domainData.ui + "item";
			annotation_ui = domainData.annotation_ui;

			// Add image and then load anotorious
			setImage()
			.then(function(metadata) {addAnnotationFields(metadata)});
			maybeRunExperiment();
			populateUI();
			populateNavbar(userName, [{link:"profile.html", name:"Profile"}]);
		};
		domainSettings = domainSettings(domain, onDomain);
	};
	// If user is not logged go to intro page
	var onDismissal = function() {document.location.href="intro.html";};
	logUserIn(onLoggedIn, onDismissal);
}

function setImage() {
	return $.getJSON("metadata", {uri:uri})
	.then(function(metadata){
		// Set id image
		page.imageId = "itemImg" + generateIdFromUri(uri);
		$(".itemImg").attr("id", page.imageId);
		// Return info for anotorious
		return metadata;
	});
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
	.done(function(labels){
		document.title = labels.title;
		initLabels(labels);

		// Only show path when cluster is available TODO: remove ugly check for undefined
		if((localStorage.getItem("currentCluster") !== null) && (localStorage.getItem("currentCluster") !== "undefined") && !(experiment === "random"))
			addPath();
		addButtonEvents();
		events();
	});
	displayMetadata();
	displayAnnotations();
}

function initLabels(data) {
	$("#itemBtnPrevious").append(data.itemBtnPrevious);
	$("#itemBtnNext").prepend(data.itemBtnNext);
	$("#navbarBtnRecommend").append(data.navbarBtnRecommend);
	$("#navbarBtnSearch").append(data.navbarBtnSearch);
	vntFirstTitle = data.vntFirstTitle;
	vntFirstText = data.vntFirstText;
	// Add next to optional experiment navigation
	$("#itemBtnExperimentNext").prepend(data.itemBtnNext);
}

function events() {
	$.getJSON("annotations", {uri:user, type:"user"})
	.done(function(annotations){
		if(annotations.length===0) {
			alertMessage(vntFirstTitle, vntFirstText, 'success');
		}
	});
}

function addPath() {
	query = localStorage.getItem("query");
	var cluster = JSON.parse(localStorage.getItem("currentCluster"));
	$("#path").append(pathHtmlElements(cluster.path));
	unfoldPathEvent("#path", cluster.path);
	addNavigationButtonEvents();
}

function addButtonEvents() {
	$("#navbarBtnRecommend").click(function() {
		document.location.href="results.html" + "?user=" + user;
	});
	// Search on pressing enter
	$("#navbarInpSearch").keypress(function(event) {
		if (event.which == 13) {
			var query = encodeURIComponent($("#navbarInpSearch").val());
			document.location.href="results.html?query=" + query;
		}
	});
	$("#navbarBtnSearch").click(function() {
		var query = encodeURIComponent($("#frmSearch").val());
		document.location.href="results.html?query=" + query;
	});
}

function addNavigationButtonEvents() {
	var index = parseInt(localStorage.getItem("itemIndex"));
	var cluster = JSON.parse(localStorage.getItem("currentCluster"));
	var items = cluster.items;

	if(index === 0) {
		$("#itemBtnPrevious").attr("disabled", "disabled");
	} else {
		$("#itemBtnPrevious").click(function() {
			localStorage.setItem("itemIndex", index - 1);
			document.location.href= "annotate.html?uri=" + items[index -1].uri;
		});
	}

	if(index === items.length-1) {
		$("#itemBtnNext").attr("disabled", "disabled");
	} else {
		$("#itemBtnNext").click(function() {
			localStorage.setItem("itemIndex", index + 1);
			document.location.href= "annotate.html?uri=" + items[index + 1].uri;
		});
	}
}

function addAnnotationFields(metadata) {
	// Retrieve the fields that should be added (based on save_user_info)
	$.getJSON("annotation_fields",
			  {locale:locale,
			   domain:domain,
		   	   annotation_ui:annotation_ui})
	.then(function(fields) {
		// Add fields whole image
		for (var i=0; i<fields.whole_fields.length; i++) {
			// Create new field object
			var wholeField = new Field(
				fields.whole_fields[i],
				{	id: "whole" + generateIdFromUri(fields.whole_fields[i].uri),
					fragment: false,
					target: uri,
				 	targetImage: metadata.image_uri,
					user: user,
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
		for (var i=0; i<fields.fragment_fields.length; i++) {
			// Create new field object
			var fragmentField = new Field(
				fields.fragment_fields[i],
				{	id: "fragment" + generateIdFromUri(fields.fragment_fields[i].uri),
					fragment: true,
					target: uri,
				 	targetImage: metadata.image_uri,
					user: user,
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

function displayMetadata() {
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

	for(var i=0; i<metadata.properties.length; i++) {
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

function displayAnnotations() {
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


	for(var i=0; i<annotations.annotations.length; i++) {
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

function maybeRunExperiment() {
	// Hide some elements during an experiment
	if(experiment !== "none") {
		// Hide path if on annotate page
		$("#itemDivClusterNavigation").hide();
		// Don't show metadata
		page.showMetadata = false;
		// Don't show annotations of others
		page.showAnnotations = false;
		// Add big next button
		addExperimentNavigation();
	}
}

function addExperimentNavigation() {
	$("#itemDivMetadata").before(
		$.el.div({'class':'row',
				  'id':'itemDivNavigationExperiment'},
			$.el.button({'class':'btn btn-primary',
						 'id':'itemBtnExperimentNext'})
		)
	);

	// Get the number of objects annotated
	$.getJSON("annotations", {uri:user, type:"user"})
	.done(function(annotations){
		var numberAnnotated = annotations.length;

		// Switch AB setting after 5 annotations
		if(numberAnnotated == 5) {
			var AorB = getAOrB();

			if(AorB === "recommend")
				setAOrB("random");
			if(AorB === "random")
				setAOrB("recommend")
		}

		// Add click event to navigation button
		$("#itemBtnExperimentNext").click(function() {
			// Go to thank you page after 20 annotations else results
			if(numberAnnotated == 500) {
				document.location.href="end.html";
			} else {
				var items = JSON.parse(localStorage.getItem("currentCluster"));
				var index = items.indexOf(uri);
				var next = index + 1;
				document.location.href="annotate.html" + "?uri=" + items[next];
			}
		});
	});
}
