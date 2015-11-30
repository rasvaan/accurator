/*******************************************************************************
Accurator Annotate
Code for extending functionallity annotation page.
*******************************************************************************/
var query, locale, experiment, domain, user, ui, uri;
var annotateHdrFirst, annotateTxtFirst;

display = {
	showMetadata: true,
	showAnnotations: true,
}

function annotateInit() {
	locale = getLocale();
	domain = getDomain();
	experiment = getExperiment();
	uri = getParameterByName("uri");

	populateFlags(locale);

	// Make sure user is logged in
	onLoggedIn = function(loginData) {
		setLinkLogo("profile");

		// Get domain settings before populating ui
		onDomain = function(domainData) {
			user = loginData.user;
			var userName = getUserName(loginData.user);
			ui = domainData.ui + "annotate";

			maybeRunExperiment();
			populateUI();
			populateNavbar(userName, [{link:"profile.html", name:"Profile"}]);
		};
		domainSettings = domainSettings(domain, onDomain);
	};
	// If user is not logged go to intro page
	onDismissal = function(){document.location.href="intro.html";};
	logUserIn(onLoggedIn, onDismissal);
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
	.done(function(labels){
		document.title = labels.title;
		initLabels(labels);
		initImage();
		// Only show path when cluster is available
		if(localStorage.getItem("currentCluster") !== null)
			addPath();
		addButtonEvents();
		events();
	});
	metadata();
	annotations();
}

function initLabels(data) {
	$("#annotateBtnPrevious").append(data.annotateBtnPrevious);
	$("#annotateBtnNext").prepend(data.annotateBtnNext);
	$("#navbarBtnRecommend").append(data.navbarBtnRecommend);
	$("#navbarBtnSearch").append(data.navbarBtnSearch);
	annotateHdrFirst = data.annotateHdrFirst;
	annotateTxtFirst = data.annotateTxtFirst;
	// Add next to optional experiment navigation
	$("#annotateBtnExperimentNext").prepend(data.annotateBtnNext);
}

function initImage() {
	$.getJSON("metadata", {uri:uri})
	.done(function(metadata){
		$(".annotationImage").attr("src", metadata.image);
	});
}

function events() {
	$.getJSON("annotations", {uri:user, type:"user"})
	.done(function(annotations){
		uris = annotations;
		if(annotations.length===0) {
			alertMessage(annotateHdrFirst, annotateTxtFirst, 'success');
		}
	});
}

function addPath() {
	query = localStorage.getItem("query");
	var cluster = JSON.parse(localStorage.getItem("currentCluster"));
	$("#path").append(pathHtmlElements(cluster.path));
	unfoldPathEvent("#path", cluster.path);
	addClusterNavigationButtonEvents();
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
		var query = encodeURIComponent($("#navbarInpSearch").val());
		document.location.href="results.html?query=" + query;
	});
}

function addClusterNavigationButtonEvents() {
	var index = parseInt(localStorage.getItem("itemIndex"));
	var cluster = JSON.parse(localStorage.getItem("currentCluster"));
	var items = cluster.items;

	if(index === 0) {
		$("#annotateBtnPrevious").attr("disabled", "disabled");
	} else {
		$("#annotateBtnPrevious").click(function() {
			localStorage.setItem("itemIndex", index - 1);
			document.location.href= "annotate_image.html?uri=" + items[index -1].uri;
		});
	}

	if(index === items.length-1) {
		$("#annotateBtnNext").attr("disabled", "disabled");
	} else {
		$("#annotateBtnNext").click(function() {
			localStorage.setItem("itemIndex", index + 1);
			document.location.href= "annotate_image.html?uri=" + items[index + 1].uri;
		});
	}
}

function metadata() {
	if(display.showMetadata){
		// Get metadata from server
		$.getJSON("metadata", {uri:uri})
		.done(function(metadata){
			appendMetadataWell(metadata);
		});
	}
}

function appendMetadataWell(metadata) {
	$("#annotateDivMetadata").append(
		$.el.div({'class':'row'},
			$.el.div({'class':'col-md-10 col-md-offset-1'},
				$.el.div({'class':'well well-sm'},
				  $.el.h4('Showing metadata for ' + metadata.title),
					$.el.dl({'class':'dl-horizontal',
							 'id':'annotateLstMetadata'})))));

	for(var i=0; i<metadata.properties.length; i++) {
		$("#annotateLstMetadata").append(
			$.el.dt(metadata.properties[i].predicate_label));
		$("#annotateLstMetadata").append(
			$.el.dd(
				$.el.a({'class':'r_undef',
					    'href':'results.html?query=' +
							   metadata.properties[i].object_label},
					metadata.properties[i].object_label)));
	}
}

function annotations() {
	// Get annotations from server
	if(display.showAnnotations){
		$.getJSON("annotations", {uri:uri, type:"object"})
		.done(function(annotations){
			if(annotations.annotations.length > 0){
				annotationWell(annotations);
			}
		});
	}
}

function annotationWell(annotations) {
	$("#annotateDivMetadata").append(
		$.el.div({'class':'row'},
			$.el.div({'class':'col-md-10 col-md-offset-1'},
				$.el.div({'class':'well well-sm'},
				  $.el.h4('Annotations for ' + annotations.title),
					$.el.dl({'class':'dl-horizontal',
							 'id':'annotateLstAnnotations'})))));


	for(var i=0; i<annotations.annotations.length; i++) {
		$("#annotateLstAnnotations").append(
			$.el.dt(annotations.annotations[i].field));
		$("#annotateLstAnnotations").append(
			$.el.dd(
				$.el.a({'class':'r_undef',
					    'href':'results.html?query=' +
							   annotations.annotations[i].body},
					annotations.annotations[i].body)));
	}
}

function maybeRunExperiment() {
	// Hide some elements during an experiment
	if(experiment !== "none") {
		// Hide path if on annotate page
		$("#annotateDivNavigation").hide();
		// Don't show metadata
		display.showMetadata = false;
		// Don't show annotations of others
		display.showAnnotations = false;
		// Add big next button
		addExperimentNavigation();
	}
}

function addExperimentNavigation() {
	$("#annotateDivMetadata").before(
		$.el.div({'class':'row',
				  'id':'annotateDivExperimentNavigation'},
			$.el.button({'class':'btn btn-primary',
						 'id':'annotateBtnExperimentNext'})
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
		$("#annotateBtnExperimentNext").click(function() {
			// Go to thank you page after 20 annotations else results
			if(numberAnnotated == 10) {
				document.location.href="end.html";
			} else {
				document.location.href="results.html" + "?user=" + user;
			}
		});
	});
}
