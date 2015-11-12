/*******************************************************************************
Accurator Annotate
Code for extending functionality annotation page.
*******************************************************************************/
var query, locale, experiment, domain, user, ui, uri;
var vntFirstTitle, vntFirstText;

displayOptions = {
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
	annotationFields();
	metadata();
	annotations();
}

function initLabels(data) {
	$("#btnPrevious").append(data.btnPrevious);
	$("#btnNext").prepend(data.btnNext);
	$("#btnAnnotateRecommend").append(data.btnAnnotateRecommend);
	$("#btnAnnotateSearch").append(data.btnAnnotateSearch);
	vntFirstTitle = data.vntFirstTitle;
	vntFirstText = data.vntFirstText;
	// Add next to optional experiment navigation
	$("#btnExperimentNext").prepend(data.btnNext);
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
			alertMessage(vntFirstTitle, vntFirstText, 'success');
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
	$("#btnAnnotateRecommend").click(function() {
		document.location.href="results.html" + "?user=" + user;
	});
	// Search on pressing enter
	$("#frmSearch").keypress(function(event) {
		if (event.which == 13) {
			var query = encodeURIComponent($("#frmSearch").val());
			document.location.href="results.html?query=" + query;
		}
	});
	$("#btnAnnotateSearch").click(function() {
		var query = encodeURIComponent($("#frmSearch").val());
		document.location.href="results.html?query=" + query;
	});
}

function addClusterNavigationButtonEvents() {
	var index = parseInt(localStorage.getItem("itemIndex"));
	var cluster = JSON.parse(localStorage.getItem("currentCluster"));
	var items = cluster.items;

	if(index === 0) {
		$("#btnPrevious").attr("disabled", "disabled");
	} else {
		$("#btnPrevious").click(function() {
			localStorage.setItem("itemIndex", index - 1);
			document.location.href= "annotate.html?uri=" + items[index -1].uri;
		});
	}

	if(index === items.length-1) {
		$("#btnNext").attr("disabled", "disabled");
	} else {
		$("#btnNext").click(function() {
			localStorage.setItem("itemIndex", index + 1);
			document.location.href= "annotate.html?uri=" + items[index + 1].uri;
		});
	}
}

function annotationFields() {
	var id = "PageType";
	var label = "Page type";

	// Add field
	$("#annotationFields").append(annotationField(id, label));

	// Get autocomplete alternatives
	var filter = JSON.stringify({scheme:"http://purl.org/vocab/nl/ubvu/BiblePageConceptScheme"});
	var labelRank = "['http://www.w3.org/2004/02/skos/core#prefLabel'-1]";
	$.getJSON("api/autocomplete",
		{q:"pag",
		 filter:filter,
		 labelrank:labelRank,
		 method:"all",
	 	 locale:locale})
	.done(function(alternatives){
		// Add typeahead
		addTypeAhead(id, alternatives);
		getInputAnnotationField(id);
	});
}

function annotationField(id, label) {
	return	$.el.div({'class':'form-group'},
				$.el.label({'for':'annotateInp' + id,
							'id':'annotateLbl' + id},
						   label),
				$.el.input({'type':'text',
						    'class':'form-control typeahead',
							'id':'annotateInp' + id})
	);
}

function addTypeAhead(id, alternatives){
	var array = getAlternativeArray(alternatives.results);
	console.log(array);
	$('#annotateInp' + id).typeahead({
		hint: true,
		highlight: true,
		minLength: 1
	},
	{
		name: 'alternatives',
		source: substringMatcher(array)
	});
}

function getAlternativeArray(alternatives) {
	var array = [];
	for(var i=0; i<alternatives.length; i++)
		array[i] = alternatives[i].label;
	return array;
}


var substringMatcher = function(strs) {
	return function findMatches(q, cb) {
		var matches, substringRegex;

		// an array that will be populated with substring matches
		matches = [];

		// regex used to determine if a string contains the substring `q`
		substrRegex = new RegExp(q, 'i');

		// iterate through the pool of strings and for any string that
		// contains the substring `q`, add it to the `matches` array
		$.each(strs, function(i, str) {
		  if (substrRegex.test(str)) {
		    matches.push(str);
		  }
		});

		cb(matches);
	};
};

function getInputAnnotationField(id) {
	$('#annotateInp' + id).bind('typeahead:select', function(ev, suggestion) {
		saveAnnotation(suggestion);
	});
}

function saveAnnotation(suggestion){
	console.log('Selection: ' + suggestion);
}

function metadata() {
	if(displayOptions.showMetadata){
		// Get metadata from server
		$.getJSON("metadata", {uri:uri})
		.done(function(metadata){
			appendMetadataWell(metadata);
		});
	}
}

function appendMetadataWell(metadata) {
	$("#metadata").append(
		$.el.div({'class':'row'},
			$.el.div({'class':'col-md-10 col-md-offset-1'},
				$.el.div({'class':'well well-sm'},
				  $.el.h4('Showing metadata for ' + metadata.title),
					$.el.dl({'class':'dl-horizontal',
							 'id':'metadataList'})))));

	for(var i=0; i<metadata.properties.length; i++) {
		var encodedQuery = encodeURIComponent(metadata.properties[i].object_label);
		$("#metadataList").append(
			$.el.dt(metadata.properties[i].predicate_label));
		$("#metadataList").append(
			$.el.dd(
				$.el.a({'class':'r_undef',
					    'href':'results.html?query=' + encodedQuery},
					metadata.properties[i].object_label)));
	}
}

function annotations() {
	// Get annotations from server
	if(displayOptions.showAnnotations){
		$.getJSON("annotations", {uri:uri, type:"object"})
		.done(function(annotations){
			if(annotations.annotations.length > 0){
				$("#metadata").append(annotationWell(annotations));
			}
		});
	}
}

function annotationWell(annotations) {
	$("#metadata").append(
		$.el.div({'class':'row'},
			$.el.div({'class':'col-md-10 col-md-offset-1'},
				$.el.div({'class':'well well-sm'},
				  $.el.h4('Annotations for ' + annotations.title),
					$.el.dl({'class':'dl-horizontal',
							 'id':'annotationList'})))));


	for(var i=0; i<annotations.annotations.length; i++) {
		var encodedQuery = encodeURIComponent(annotations.annotations[i].body);
		$("#annotationList").append(
			$.el.dt(annotations.annotations[i].field));
		$("#annotationList").append(
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
		$("#clusterNavigation").hide();
		// Don't show metadata
		displayOptions.showMetadata = false;
		// Don't show annotations of others
		displayOptions.showAnnotations = false;
		// Add big next button
		addExperimentNavigation();
	}
}

function addExperimentNavigation() {
	$("#metadata").before(
		$.el.div({'class':'row',
				  'id':'experimentNavigation'},
			$.el.button({'class':'btn btn-primary',
						 'id':'btnExperimentNext'})
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
		$("#btnExperimentNext").click(function() {
			// Go to thank you page after 20 annotations else results
			if(numberAnnotated == 10) {
				document.location.href="end.html";
			} else {
				document.location.href="results.html" + "?user=" + user;
			}
		});
	});
}
