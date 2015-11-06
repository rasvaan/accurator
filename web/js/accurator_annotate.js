/*******************************************************************************
Accurator Annotate
Code for extending functionallity annotation page. Most of the content and
javascript of this page is generated based on the image_annotation/applications/
annotation.pl code and the other halve comes from the result.js of
cluster_search_ui.
*******************************************************************************/
var query, locale, experiment, domain, user, ui, uri;
var vntFirstTitle, vntFirstText;

displayOptions = {
	showMetadata: true,
	showAnnotations: true,
	metadataLinkBase: 'results.html?query='
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
	// Use cluster_search_ui result.js code to show the result
	showResult(uri);
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
		console.log(metadata);
		$(".annotationImage").attr("src", metadata.image_link);
	});
}

function events() {
	$.getJSON("recently_annotated", {user:user})
	.done(function(annotations){
		uris = annotations.uris;
		if(uris.length===0) {
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
			document.location.href= "annotate_image.html?uri=" + items[index -1].uri;
		});
	}

	if(index === items.length-1) {
		$("#btnNext").attr("disabled", "disabled");
	} else {
		$("#btnNext").click(function() {
			localStorage.setItem("itemIndex", index + 1);
			document.location.href= "annotate_image.html?uri=" + items[index + 1].uri;
		});
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
	$.getJSON("recently_annotated", {user:user})
	.done(function(annotations){
		var numberAnnotated = annotations.uris.length;

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
