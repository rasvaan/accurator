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
	showControls: true
}

// Initialize page
function resultsInit() {
	var locale = getLocale();
	var domain = getDomain();

	populateFlags(locale);
	clearLocalStorage("uris"); // will be generating new clusters
	clearLocalStorage("path");

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
		var ui, target, labels;
		var user = userData.user;
		var userName = getUserName(user);
		var realName = userData.real_name;

		setLinkLogo("profile");
		populateNavbar(userName, [{link:"profile.html",	name:"Profile"}], locale);

		domainSettings(domain)
		.then(function(domainData) {
			ui = domainData.ui + "results";
			target = domainData.target;

			return getLabels(locale, ui);
		})
		.then(function(labelData) {
			labels = initLabels(labelData);
			labels.realName = realName; // Add realname to labels for rendering
			addButtonEvents(user);

			return events(user, labels);
		})
		.then(function() {
			// Provide results based on query, recommend something based on
			// the expertise of the retrieved user or, if none of these, show
			// just random results
			results(target, labels);
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
		resultsLblCluster: labelData.resultsLblCluster,
		resultsLblList: labelData.resultsLblList
	};

	return labels;
}

// Add button events in the navbar
function addButtonEvents(user) {
	$("#navbarBtnRecommend").click(function() {
		document.location.href = "results.html?user=" + user;
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
function results(target, labels) {
	var query = getParameterByName("query"); // get query from url when present
	var userQuery = getParameterByName("user"); // get user from url when present
	var recommendBoolean = true; // don't do random stuff

	if(query) {
		// results based on the user query
		search(query, labels);
	} else if(recommendBoolean) {
		// recommendations based on the expertise of the user
		// first recommended results are shown, then random results
		query = "expertise";
		recommend(query, labels, target);
	} else {
		// random results
		query = "random";
		random(query, labels, target, 20);
	}
}

// Get results based on the user query
function search(query, labels) {
	$.getJSON("cluster_search_api", {query:query})
	.then(function(data) {
		$(document).prop('title', labels.resultsHdrResults + query);
		var clusters = processClusters(data, labels, query);
		// if there are any clusters retrieved, then draw results
		if (clusters.length > 0) {
			controls(clusters, labels); // add control buttons to change layout
			drawResults(clusters);
		}
	}, function(data) {
		statusMessage(labels.resultsTxtError, data.responseText);
	});
}

// Get results based on the expertise of the user and, afterwards, a number of
// random items that have not yet been annotated
function recommend(query, labels, target) {
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
		controls(clusters, labels); // add control buttons to change layout
		drawResults(clusters);
	});
}

// Get random items
function random(query, labels, target, noResults) {
	randomCluster(target, noResults)
	.then(function(cluster) {
		var clusters = [];
		clusters[0] = cluster;
		controls(clusters, labels);
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
		statusMessage(labels.resultsTxtNoResults + query);
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
	// Get a list of random items
	return $.getJSON("recommendation", {
		strategy:'random',
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

		thumbnail.setClickEvent(cluster.items[itemIndex].link, cluster.uris, "path");
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
		$.el.div({'class':'row'},
			$.el.div({'class':'col-lg-10 col-md-offset-1'},
				$.el.h3(header)),
			$.el.div({'class':'row'},
				$.el.div({'class':'col-md-10 col-md-offset-1'},
					text)))
	);
}

/*******************************************************************************
Controls
Code for adding buttons controlling the layout
*******************************************************************************/

// Add the container for the controls that change the display of the items
function controls(results, labels) {
	if(display.showControls) {
		$("#resultsDiv").prepend(
			$.el.div({'class':'row'},
				$.el.div({'class':'col-md-12 resultsDivControls'}))
		);
		resultLayoutButtons(results, labels);
	}
}

// Add the buttons and the click functionality for changing the display
function resultLayoutButtons(results, labels) {
	$(".resultsDivControls").append(
		$.el.div({'class':'btn-group'},
			$.el.button({'class':'btn btn-default',
						 'id':'resultsBtnLayout'}))
	);
	setLayoutButton(labels);

	$("#resultsBtnLayout").click(function() {
		var resultNodes = document.getElementById("resultsDiv");

		// use pure javascript removal in order to not remove attached events
		while(resultNodes.firstChild) {
    		resultNodes.removeChild(resultNodes.firstChild);
		}

		display.layout = (display.layout === "list") ? "cluster" : "list";
		controls(results, labels);
		drawResults(results);
	});
}

// Set the text of the display button depending on the view that is rendered
function setLayoutButton(labels) {
	if(display.layout === "list") {
		$("#resultsBtnLayout").html(
			$.el.span(labels.resultsLblCluster + ' ',
			$.el.span({'class':'glyphicon glyphicon-th-large'}))
		);
	} else {
		$("#resultsBtnLayout").html(
			$.el.span(labels.resultsLblList + ' ',
			$.el.span({'class':'glyphicon glyphicon-th-large'}))
		);
	}
}
