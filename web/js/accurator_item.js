/*******************************************************************************
Accurator Item
Code for extending functionality item page.
*******************************************************************************/
var query, locale, experiment, domain, user, ui, annotation_ui, uri;
var vntFirstTitle, vntFirstText;
var annotoriousFields;

var displayOptions = {
	showMetadata: true,
	showAnnotations: true,
}

var page = {
	imageId: null, // Set on init image
	fieldContainerId: "itemDivAnnotoriousFields" // Id container containing fields

}

function itemInit() {
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
			ui = domainData.ui + "item";
			annotation_ui = domainData.annotation_ui;

			// Add image and then load anotorious
			imagePromise().then(function(metadata){addAnotorious(metadata)});
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

function imagePromise() {
	return $.getJSON("metadata", {uri:uri})
	.done(function(metadata){
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
		uris = annotations;
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

function addAnotorious(metadata) {
	console.log("1. addAnotorious, retrieve fields");
	// Retrieve the fields that should be added (based on save_user_info)
	$.getJSON("annotation_fields",
			  {locale:locale,
			   domain:domain,
		   	   annotation_ui:annotation_ui})
	.done(function(fields){
		console.log("1.1 addAnotorious, iterate through whole fields:", fields.whole_fields);
		// Add fields whole image
		for (var i=0; i<fields.whole_fields.length; i++) {
			// $("#itemFrmAnnotationFields").append(
			// 	annotationField(fields.whole_fields[i])
			// );
		}

		console.log("1.2 addAnotorious, create dom container for fragment fields with id:", page.fieldContainerId);
		// Add hidden container for fields if there are fragment fields
		if (fields.fragment_fields.length > 0) {
			$(".itemDivHidden").append($.el.div({'id':page.fieldContainerId}));
			// Set fields attribute for annotorious deniche
			$("#" + page.imageId).attr("fields", page.fieldContainerId);
		}

		console.log("1.3 addAnotorious, iterate through fragment fields:", fields.fragment_fields);
		// Add fields to hidden dom elements for annotorious
		for (var i=0; i<fields.fragment_fields.length; i++) {
			var fieldDef = fields.fragment_fields[i];

			// Create new field object
			var field = new Field(
				fieldDef,
				{	id:generateIdFromUri(fieldDef.uri),
					target:uri,
				 	targetImage:metadata.image_uri,
					user:user,
			 	 	imageId:page.imageId,
					fieldsId:page.fieldContainerId
			 	}
			);
			// Append the field to div which will be embedded in annotorious
			//TODO: this should be done in deniche
			console.log("1.3.2 addAnotorious (should be deniche), add field to dom.");
			$("#" + page.fieldContainerId).append(field.node);
		}
		console.log("2. addAnotorious, add the deniche plugin, which embeds the fields in annotorious.");
		// Add the deniche plugin, which embeds the fields in annotorious
		anno.addPlugin("DenichePlugin", {});
	});
}

function displayMetadata() {
	if(displayOptions.showMetadata){
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
	if(displayOptions.showAnnotations){
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
		displayOptions.showMetadata = false;
		// Don't show annotations of others
		displayOptions.showAnnotations = false;
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
