/*******************************************************************************
Accurator Results

Page showing overview of recommender/search results. Uses a lot of code from
cluster.js, which in turn uses path.js, pagination.js and thumbnail.js
(in web/js/components).

Options:

1. SEARCH: If a user query is entered, then the page shows the results that
   match that query.
2. RECOMMENDER: If no query is entered by the user and the recommend is set to
   true, then results will contain recommendations given based on the expertise
   of the user. Also, below these recommendations, a number of random items not
   yet annotated are shown.
3. RANDOM: If no query is entered and the recommender is not used, then a random
   list of results will be shown.

Layout of the results:

1. CLUSTER VIEW: results will be grouped according to their path elements. More
   elements can belong to the same cluster and these will be shown per row.
2. LIST VIEW: results are in the form of a list with a certain number of items
   per row.

*******************************************************************************/
"use strict";

// Display options deciding how the results get displayed
var display = {
	layout: "cluster",
	numberDisplayedItems: 4,
}

// Initialize page
function resultsInit() {
	var locale = getLocale();
	var domain = getDomain();

	populateFlags(locale);

	userLoggedIn()
	.then(function(userData) {
		// user is logged in, so draw page
		drawPage(userData);
	}, function() {
		// user is not logged in, show modal
		var onDismissal = function() {
			document.location.href = "intro.html";
		};

		login(drawPage, onDismissal);
	});

	function drawPage(userData) {
		var ui, target, labels, domainData;
		var user = userData.user;
		var userName = getUserName(user);
		var realName = userData.real_name;

		setLinkLogo("profile");
		populateNavbar(userName, [{link:"profile.html",	name:"Profile"}], locale);

		domainSettings(domain)
		.then(function(data) {
			domainData = data; // enable reuse in next function
			ui = domainData.hasUI + "results";
			target = domainData.hasTarget;

			return getLabels(locale, ui);
		})
		.then(function(labelData) {
			labels = initLabels(labelData);
			labels.realName = realName; // add realname to labels for rendering
			addButtonEvents(user, target, labels, domainData);

			return events(user, labels);
		})
		.then(function() {
			// Provide results based on query, recommend something based on
			// the expertise of the retrieved user or, if none of these, show
			// just random results
			results(target, labels, domainData);
		})
	}
}

// Add retrieved labels to html elements
function initLabels(labelData) {
	$("#navbarBtnSearch").append(labelData.navbarBtnSearch);
	$("#navbarBtnRecommend").append(labelData.resultsBtnRecommend);

	var labels = {
		resultsTxtRecommendationsFor: labelData.resultsTxtRecommendationsFor,
		resultsTxtSearching: labelData.resultsTxtSearching,
		resultsHdrResults: labelData.resultsHdrResults,
		resultsHdrRecommendedResults: labelData.resultsHdrRecommendedResults,
		resultsHdrRandomResults: labelData.resultsHdrRandomResults,
		resultsHdrFirst: labelData.resultsHdrFirst,
		resultsTxtFirst: labelData.resultsTxtFirst,
		resultsTxtNoResults: labelData.resultsTxtNoResults,
		resultsTxtError: labelData.resultsTxtError,
		resultsBtnChooseSubDomain: labelData.resultsBtnChooseSubDomain,
		resultsBtnChangeSubDomain: labelData.resultsBtnChangeSubDomain,
		resultsBtnChangeExpertise: labelData.resultsBtnChangeExpertise,
		resultsLblCluster: labelData.resultsLblCluster,
		resultsLblList: labelData.resultsLblList
	};

	return labels;
}

function events(user, labels) {
	return $.getJSON("annotations", {
		uri:user,
		type:"user"
	})
	.then(function(annotations) {
		// message displayed before the first annotation is made by a user
		if (annotations.length === 0) {
			alertMessage(labels.resultsHdrFirst, labels.resultsTxtFirst, 'success');
		}
	});
}

/*******************************************************************************
Search, Recommend or Random results
*******************************************************************************/
function results(target, labels, domainData) {
	var query = getParameterByName("query"); // get query from url when present
	var userQuery = getParameterByName("user"); // get user from url when present
	var recommendBoolean = false; // do random stuff

	clearLocalStorage("uris"); // will be generating new clusters
	clearLocalStorage("path");

	if(query) {
		// results based on the user query
		search(query, labels);
	} else if(recommendBoolean) {
		// recommendations based on the expertise of the user
		// first recommended results are shown, then random results
		query = "expertise";
		recommend(query, labels, target, domainData);
	} else {
		// random results
		query = "random";
		random(query, labels, target, 20, domainData);
	}
}

// Get results based on the user query
function search(query, labels) {
	statusMessage(labels.resultsTxtSearching + query);

	$.getJSON("cluster_search_api", {query:query})
	.then(function(data) {
		$(".resultsDivStatus").remove();
		$(document).prop('title', labels.resultsHdrResults + query);
		var clusters = processClusters(data, labels, query);

		// if there are any clusters retrieved, then draw results
		if (clusters.length > 0) {
			resultLayoutButtons(clusters, labels); // add control buttons to change layout
			drawResults(clusters);
		} else {
			// tell the people nothing is found
			statusMessage(labels.resultsTxtNoResults + query);
		}
	}, function(data) {
		statusMessage(labels.resultsTxtError, data.responseText);
	});
}

// Get results based on the expertise of the user and, afterwards, a number of
// random items that have not yet been annotated
function recommend(query, labels, target, domainData) {
	var recommendation = $.getJSON("recommendation", {
		strategy:query,
		target:target
	})
	.then(function(data) {
		$(document).prop('title', labels.resultsTxtRecommendationsFor + labels.realName);
		return processClusters(data, labels, query);
	});

	var random = randomCluster(target, 10);

	$.when(recommendation, random)
	.then(function(clusters, randomCluster) {
		clusters.push(randomCluster);
		resultLayoutButtons(clusters, labels); // add control buttons to change layout
		domainButton(domainData, labels);
		drawResults(clusters);
	});
}

// Get random items
function random(query, labels, target, noResults, domainData) {
	randomCluster(target, noResults)
	.then(function(cluster) {
		var clusters = [];
		clusters[0] = cluster;
		resultLayoutButtons(clusters, labels);
		domainButton(domainData, labels);
		drawResults(clusters);
	}, function(data) {
		statusMessage(labels.resultsTxtError, data.responseText);
	});
}

/*******************************************************************************
Result population and enrichment
*******************************************************************************/
function processClusters(data, labels, query) {
	var clusters = []; // array containing cluster objects

	if (data.clusters.length === 0) {
		return clusters;
	} else {
		for (var i = 0; i < data.clusters.length; i++) {
			var uris = []; // uris of items in cluster
			var id = "cluster" + i; // id of cluster
			var path = data.clusters[i].path;

			for(var j = 0; j < data.clusters[i].items.length; j++)
				uris[j] = data.clusters[i].items[j].uri;

			clusters[i] = new Cluster(id, uris, path, query);
		}

		return clusters;
	}
}

function randomCluster(target, noResults) {
	// Get a list of ranked random items
	return $.getJSON("recommendation", {
		strategy:'ranked_random',
		number:noResults,
		target:target
	})
	.then(function(uris) {
		// create a cluster with random items
		return new Cluster("clusterRandom", uris, "random");
	});
}

/*******************************************************************************
Display of results and helper functions for the display
*******************************************************************************/
function drawResults(clusters) {
	drawRows(clusters);

	for (var i = 0; i < clusters.length; i++)
		drawCluster(clusters[i], clusters);
}

// Add rows for cluster items
function drawRows(clusters) {
	if (display.layout === "cluster") {
		for (var i = 0; i < clusters.length; i++) {
			$("#resultsDiv").append(
				clusters[i].node
			);
		}
	} else if (display.layout === "list") {
		var totalItems = itemsInClusters(clusters, clusters.length);
		var rows = getNumberOfRows(totalItems);

		for (var i = 0; i < rows; i++) {
			$("#resultsDiv").append(
				$.el.div({'class':'row',
					'id':'resultsDivThumbnails' + i})
			);
		}
	}
}

// Determine number of items up to a certain cluster
function itemsInClusters(clusters, clusterIndex) {
	var totalItems = 0;

	for (var i = 0; i < clusterIndex; i++){
		// count uris since cluster might not be enriched
		totalItems += clusters[i].uris.length;
	}

	return totalItems;
}

// Determine number of pages or rows based on the items to be shown
function getNumberOfRows(numberOfItems) {
	var numberOfRows = 0;
	var restRows = numberOfItems%display.numberDisplayedItems;

	if (restRows === 0) {
		return numberOfItems/display.numberDisplayedItems;
	} else {
		return (numberOfItems-restRows)/display.numberDisplayedItems+1;
	}
}

function drawCluster(cluster, clusters) {
	var draw = function() {
		if (display.layout === "cluster") {
			$("#resultsDiv #" + cluster.id).append(cluster.node);
		} else if (display.layout === "list") {
			displayClusterAsList(cluster, clusters);
		}
	}

	if (cluster.initialized) {
		draw();
	} else {
		cluster.init(display.numberDisplayedItems)
		.then(function() {
			draw();
		});
	}
}

// Display the items from one cluster and add them as items in a list
function displayClusterAsList(cluster, clusters) {
	// determine from where to start adding items based on the contents of the previous clusters
	var itemsAdded = itemsInClusters(clusters, clusters.indexOf(cluster));
	var allUris = mergeUrisClusters(clusters, "list");

	//for every item in this cluster, add the thumbnail in the list view
	for (var itemIndex = 0; itemIndex < cluster.uris.length; itemIndex++) {
		var rowNumber = parseInt(itemsAdded/display.numberDisplayedItems, 10);
		var rowId = "resultsDivThumbnails" + rowNumber;
		var index = itemsAdded%display.numberDisplayedItems;
		var thumbnail = new Thumbnail(
			cluster.items[itemIndex].uri,
			cluster.items[itemIndex].title,
			cluster.items[itemIndex].thumb,
			cluster.items[itemIndex].link,
			display.numberDisplayedItems
		);

		thumbnail.setClickEvent(
			cluster.items[itemIndex].link,
			cluster.uris,
			cluster.path.path
		);

		$("#" + rowId).append(thumbnail.node);
		itemsAdded++;
	}
}

// Function merging multiple clusters into one adding a new title
function mergeUrisClusters(clusters) {
	var uris = [];

	// retrieve all uris
	for (var i = 0; i < clusters.length; i++)
		uris = uris.concat(clusters[i].uris);

	return uris;
}

// Add a title for the page and print a status message within the page that
// gives more information on the progress of the search
function statusMessage(header, text){
	$(document).prop('title', header);

	$("#resultsDiv").html(
		$.el.div({'class':'row resultsDivStatus'},
			$.el.div({'class':'col-lg-10 col-md-offset-1'},
				$.el.h3(header)),
			$.el.div({'class':'row'},
				$.el.div({'class':'col-md-10 col-md-offset-1'},
					text)))
	);
}

/*******************************************************************************
Buttons
Code for adding buttons controlling the layout
*******************************************************************************/
function resultLayoutButtons(results, labels) {
	// add button
	$(".resultsDivControls").prepend(
		$.el.button({'class':'btn btn-default',
					 'id':'resultsBtnLayout'})
	);

	// add click event
	$("#resultsBtnLayout").click(function() {
		var resultNodes = document.getElementById("resultsDiv");

		// use pure javascript removal in order to not remove attached events
		while(resultNodes.firstChild) {
    		resultNodes.removeChild(resultNodes.firstChild);
		}

		display.layout = (display.layout === "list") ? "cluster" : "list";
		setLayoutButtons(results, labels); // wow, semi recursion in js...
		drawResults(results);
	});

	// add text
	setLayoutButtons(results, labels)
}


function setLayoutButtons(results, labels) {
	// set the text
	if(display.layout === "list") {
		$("#resultsBtnLayout").html(
			[$.el.span(labels.resultsLblCluster + ' '),
			$.el.span({'class':'glyphicon glyphicon-th-list'})]
		);
	} else {
		$("#resultsBtnLayout").html(
			[$.el.span(labels.resultsLblList + ' '),
			$.el.span({'class':'glyphicon glyphicon-th'})]
		);
	}
}

// Add the buttons and the click functionality for changing the display
function domainButton(domainData, labels) {
	if (domainData.subDomains) {
		// show option to select more specific subdomain
		addDomainButton(labels.resultsBtnChooseSubDomain);
		$("#resultsBtnSubDomains").click(function() {
			document.location.href = "domain.html?domain=" + domainData.domain;
		});
	} else if (domainData.requires) {
		// show button for providing expertise values
		addDomainButton(labels.resultsBtnChangeExpertise);
		$("#resultsBtnSubDomains").click(function() {
			document.location.href = "expertise.html";
		});
	} else if (domainData.superDomain) {
		// show option to select other subdomain
		addDomainButton(labels.resultsBtnChangeSubDomain);
		$("#resultsBtnSubDomains").click(function() {
			document.location.href =
				"domain.html?domain=" +
				generateDomainFromUri(domainData.superDomain);
		});
	}
}

function addDomainButton(label) {
	$(".resultsDivControls").append(
		$.el.button({
			'class':'btn btn-primary',
			'id':'resultsBtnSubDomains'},
			label
		)
	);
}

function addButtonEvents(user, target, labels, domainData) {
	// add button events in the navbar
	$("#navbarBtnRecommend").click(function() {
		// check if not already there
		if (!(document.location.href.indexOf("results.html?user=" + user)  > -1)) {
			document.location.href = "results.html?user=" + user;
		} else {
			// clear current and get new results
			$("#resultsDiv").empty();
			$("#resultsBtnLayout").remove();
			$("#resultsBtnSubDomains").remove();
			results(target, labels, domainData);
		}
	});
	// search on pressing enter
	$("#navbarInpSearch").keypress(function(event) {
		if (event.which == 13) {
			var query = encodeURIComponent($("#navbarInpSearch").val());

			document.location.href = "results.html?query=" + query;
		}
	});
	$("#navbarBtnSearch").click(function() {
		var query = encodeURIComponent($("#navbarInpSearch").val());

		document.location.href = "results.html?query=" + query;
	});
}
