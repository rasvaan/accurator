/*******************************************************************************
Accurator Results

Page showing overview of recommender/search results. Uses a lot of code from
pagination.js and thumbnail.js (in web/js/components) .

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

var randoms = [];

// Display options deciding how to results get displayed
var display = {
	layout: "cluster",
	imageFilter: "onlyImages",
	numberDisplayedItems: 4,
	showControls: true
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
		var onDismissal = function() {document.location.href="intro.html"};
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
			document.location.href="results.html?query=" + query;
		}
	});
	$("#navbarBtnSearch").click(function() {
		var query = encodeURIComponent($("#navbarInpSearch").val());
		document.location.href="results.html?query=" + query;
	});
}


function events(user, labels) {
	return $.getJSON("annotations", {uri:user, type:"user"})
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
		recommend(query, target, labels);
	} else {
		// random results
		query = "random";
		random(query, labels, target, 10);
	}
	localStorage.setItem("query", query);
}

// Get results based on the user query
function search(query, labels) {
	$.getJSON("cluster_search_api", {query:query})
	.then(function(data) {
		$(document).prop('title', labels.resultsHdrResults + query);
		var clusters = processClusters(data);
		drawResults(clusters);
		// add control buttons to change layout
		controls(labels);
	}, function(data) {
		statusMessage(labels.resultsTxtError, data.responseText);
	});
}

function processClusters(data) {
	if (data.clusters.length === 0) {
		statusMessage(labels.resultsTxtNoResults + query);
	} else {
		var clusters = []; // array containing cluster objects

		for(var i=0; i<data.clusters.length; i++) {
			var uris = []; // uris of items in cluster
			var id = "cluster" + i; // id of cluster

			for(var j = 0; j < data.clusters[i].items.length; j++)
				uris[j] = data.clusters[i].items[j].uri;

			clusters[i] = new Cluster(uris, id);
		}

		return clusters;
	}
}

function drawResults(clusters) {
	drawRows(clusters);

	for (var i=0; i<clusters.length; i++)
		drawCluster(clusters[i]);
}

// Add rows for cluster items for the list view
function drawRows(clusters) {
	if (display.layout === "cluster") {
		for (var i=0; i<clusters.length; i++) {
			$("#resultsDiv").append(
				clusters[i].node
			);
		}
	} else if (display.layout === "list") {
		var totalItems = totalItemsInClusters(clusters);
		var rows = getNumberOfRows(totalItems);

		for (var i=0; i<rows; i++) {
			$("#resultsDiv").append(
				$.el.div({'class':'row',
					'id':'thumbnailRow' + i})
			);
		}
	}
}

// Determine the total number of items from clusters
function totalItemsInClusters(clusters) {
	var totalItems = 0;

	for (var i=0; i<clusters.length; i++){
		totalItems += clusters[i].items.length;
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

function drawCluster(cluster) {
	var draw = function() {
		if (display.layout === "cluster") {
			$("#resultsDiv #" + cluster.id).append(cluster.node);
			cluster.display(display.numberDisplayedItems);
			// clusters[index].display(display.numberDisplayedItems);
		} else if (display.layout === "list") {
			console.log("should be drawing lists");
		}
	}

	if (cluster.enriched) {
		draw();
	} else {
		cluster.enrich()
		.then(function() {
			draw();
		});
	}
	// clusters[index].enrich()
	// .then(function() {
	// 	console.log(clusters[0]);
	// 	// append cluster
	// 	if (display.layout === "cluster") {
	// 		displayClusterItems(clusterId);
	// 	// add enriched clusters and rows
	// 	} else if (display.layout === "list") {
	// 		itemsAdded = displayList(clusterId, itemsAdded);
	// 	}
	// });
}

// Get results based on the expertise of the user and, afterwards, a number of
// random items that have not yet been annotated
function recommend(query, target, labels) {
	$.getJSON("recommendation", {strategy:query,
								 target:target})
	.then(function(data){
		// retrieve clusters
		clusters = data.clusters;
		//localStorage.setItem("clusters", JSON.stringify(clusters));

		// enrich retrieved clusters if any
		if(clusters.length == 0){
			statusMessage(labels.resultsTxtNoResults, labels.realName);
		} else {
			statusMessage(labels.resultsHdrRecommendedResults);

			// enrich the retrieved clusters
			enrichClusters(query);
		}

		// populate random elements in the previously added random cluster
		random(query, target, 10);

		// set page title
		$(document).prop('title', labels.resultsTxtRecommendationsFor + labels.realName);

		// Add control buttons to change layout
		controls(labels);
	}, function(data) {
		statusMessage(labels.resultsTxtError, data.responseText);
	})
}

// Get random items
function random(query, labels, target, noResults) {
	// Get a list of random items
	$.getJSON("recommendation", {strategy:'random',
								 number:noResults,
								 target:target})
	.then(function(uris){
		// populate the page with random
		randoms = uris;
		// TODO add in local storage?!
		// localStorage.setItem("randoms", JSON.stringify(randoms));

		// enrich retrieved clusters if any
		if(randoms.length == 0){
			statusMessage(labels.resultsTxtNoResults, query);
		} else {
			statusMessage(labels.resultsHdrRandomResults);

			if (query === "expertise"){
				// set page title
				$(document).prop('title', labels.resultsTxtRecommendationsFor + labels.realName);
			}

			var noRandomItems = randoms.length;

			if (query === "expertise" && display.layout === "cluster") {
				addRandomPath();
			} else {
				// add rows for random objects and display them as a list
				addRandomRows(noRandomItems);
			}

			// enrich random objects
			enrichRandoms(randoms)
			.then(function(){
				// TODO add pagination here for cluster items and add cluster items
				if (query === "expertise" && display.layout === "cluster"){
					$("#randoms").append(pagination(getNoOfPagesOrRows(noRandomItems),
							randoms, "randoms"));
					displayRandomCluster(0);
				}
				else {
					displayRandomList();
				}
			});
		}
	}, function(data) {
		statusMessage(labels.resultsTxtError, data.responseText);
	});
}

/*******************************************************************************
Result population and enrichment
*******************************************************************************/

// function enrichClusters(query, labels) {
	// set page title
	// $(document).prop('title', labels.resultsHdrResults + query);

	// if the display is the list view, the rows that are needed can be first
	// created and after the enrichment is done, these can be further populated
	// if (display.layout === "list") {
	// 	var totItems = totalItemsInClusters();
	//
	// 	// add rows for every cluster item
	// 	addRows(totItems);
	// }
	// var itemsAdded = 0;

	//for every cluster item
// 	for(var i = 0; i < clusters.length; i++) {
// 		if(display.layout === "cluster") {
// 			displayClusterHeader(i);
// 		}
// 		var uris = [];
//
// 		// enrich every item in the cluster
// 		for(var j = 0; j < clusters[i].items.length; j++) {
// 			uris[j] = clusters[i].items[j].uri;
// 		}
//
// 		//when a cluster item finished being enriched, display it
// 		enrichCluster(uris, i)
// 		.then(function (clusterId){
//   		  	// add enriched clusters and pagination
// 			if (display.layout === "cluster"){
// 				displayClusterItems(clusterId);
// 			// add enriched clusters and rows
// 			} else if (display.layout === "list"){
// 				itemsAdded = displayList(clusterId, itemsAdded);
// 			}
// 		});
// 	}
// }

// Enrichment of one cluster item
// function enrichCluster(uris, clusterId){
// 	var json = {"uris":uris};
//
// 	return $.ajax({type: "POST",
// 			url: "metadata",
// 			contentType: "application/json",
// 			data: JSON.stringify(json)})
// 	.then(function(data) {
// 		   // replace cluster items array with enriched ones
// 		   clusters[clusterId].items = processEnrichment(data);
// 		   return clusterId;
// 	 });
// }

// Enrich one image element in the cluster adding an image, a link where it can
// be (further) annotated and a title
// function processEnrichment(data) {
// 	var enrichedItems = [];
//
// 	for(var i=0; i<data.length; i++) {
// 		enrichedItems[i] = {};
// 		var uri = data[i].uri;
// 		enrichedItems[i].uri = uri;
// 		enrichedItems[i].thumb = data[i].thumb;
// 		enrichedItems[i].link = "item.html?uri=" + uri;
// 		enrichedItems[i].title = truncate(data[i].title, 60);
// 	}
// 	return enrichedItems;
// }

// Enrichment of one random object
function enrichRandoms(uris) {
	// clear results div and reset rows
	// $("#resultsDiv").children().remove();

	var json = {"uris":uris};

	return $.ajax({type: "POST",
		url: "metadata",
		contentType: "application/json",
		data: JSON.stringify(json)})
	.then(function(data) {
		// Replace cluster items with enriched ones
		randoms = processEnrichment(data);
	});
}

/*******************************************************************************
Display of results and helper functions for the display
*******************************************************************************/





// Display the cluster header: the html rows that contain one cluster and
// the path of the cluster
// function displayClusterHeader(clusterId) {
// 	$("#resultsDiv").append(
// 		$.el.div({'class':'well well-sm',
// 				  'id':'cluster' + clusterId})
// 	);
// 	addPath(clusterId, clusters[clusterId].path, localStorage.query);
// }

// Add the path elements for one cluster
function addPath(clusterId, uris, query) {
	// Get labels from server
	var json = {"uris":uris, "type":"label"};

	return $.ajax({type: "POST",
		url: "metadata",
		contentType: "application/json",
		data: JSON.stringify(json)})
	.then(function (labels) {
		var pathElements = [];

		for(var i = 0; i < uris.length; i++){
			pathElements[i] = {uri:uris[i], label:truncate(labels[i], 50)};
		}

		pathElements.reverse();
		var path = new Path(uris, labels, pathElements);

		$("#cluster" + clusterId).prepend(path.htmlSimple);
		path.unfoldEvent("#cluster" + clusterId, query);
	});
}

// Display the items in one cluster with pagination (if necessary)
// function displayClusterItems(clusterId){
// 	var noPages = getNoOfPagesOrRows(clusters[clusterId].items.length);
//
// 	$("#cluster" + clusterId).append(pagination(noPages, clusters[clusterId].items, "cluster", clusterId));
// 	thumbnails(clusterId);
// }

// Add rows for cluster items for the list view
// function addRows(totItems){
// 	var noRows = getNoOfPagesOrRows(totItems);
//
// 	for (var i = 0; i < noRows; i++){
// 		$("#resultsDiv").append(
// 				$.el.div({'class':'row',
// 						 'id':'thumbnailRow' + i})
// 		);
// 	}
// }

// Display the items from one cluster and add them as items in a list
function displayList(clusterId, itemsAdded){
		//for every item in this cluster, add the thumbnail in the list view
		for(var clusterItem = 0; clusterItem < clusters[clusterId].items.length; clusterItem++) {
			var id = getId(clusters[clusterId].items[clusterItem].uri);
			var rowId = parseInt(itemsAdded/display.numberDisplayedItems, 10);
			var index = itemsAdded%display.numberDisplayedItems;

			$("#thumbnailRow" + rowId).append(thumbnail(clusters[clusterId].items[clusterItem]));
			addListClickEvent(id, clusters[clusterId].items[clusterItem].link, rowId, index, clusterId);
			itemsAdded++;
		}
		return itemsAdded;
}

// Add thumbnail click event
function addListClickEvent(id, link, rowId, index, clusterId) {
	$("#thumbnailRow" + rowId  + " #" + id).click(function() {
		//Add info to local storage to be able to save context
		localStorage.setItem("itemIndex", index);
		localStorage.setItem("rowId", rowId);
		localStorage.setItem("currentCluster", JSON.stringify(clusters[clusterId]));
		//TODO check here + this is already done for the cluster, right?
		// if((clusterId+1) == clusters.length)
		// 	localStorage.setItem("query", "random");
		document.location.href = link;
	});
}

// Add rows for random items
function addRandomRows(totItems){
	var noRows = getNoOfPagesOrRows(totItems);

	// add rows for thumbnails
    for (var i = 0; i < noRows; i++){
		// display as a list
		$("#resultsDiv").append(
			$.el.div({'class':'row',
             		  'id':'thumbnailRandomRow' + i})
		);
    }
}

// Add path node for random objects
function addRandomPath() {
	// display path for random cluster only when cluster view is set and for
	// user recommendations
	$("#resultsDiv").append(
		$.el.div({'class':'well well-sm',
						 'id':'randoms'},
			$.el.div({'class':'row path'},
				$.el.div({'class':'col-md-12'},
					$.el.h4(
						$.el.span({'class':'path-label path-literal'},
								 "random objects")))))
	);
}

// Add rows for random items that are displayed after recommended ones
function addRandomNodes(totItems){
	var noPagesOrRows = getNoOfPagesOrRows(totItems);

	// add rows for thumbnails
    for (var i = 0; i < noPagesOrRows; i++){
		// display as a list
		$("#randoms").append(
			$.el.div({'class':'row',
             		  'id':'thumbnailRandomRow' + i})
		);
    }
}

// Display the random list of items
function displayRandomList(){
	var noRows = getNoOfPagesOrRows(randoms.length);
	var stop = display.numberDisplayedItems;
	var itemsAdded = 0;

	// populate rows of random
	for (var rowId = 0; rowId < noRows; rowId++){
		for (var index = 0; index < stop; index++){
			if (itemsAdded < randoms.length){
				var id = getId(randoms[itemsAdded].uri);

				$("#thumbnailRandomRow" + rowId).append(thumbnail(randoms[itemsAdded]));
				addRandomClickEvent(id, randoms[itemsAdded].link, rowId, index);
				itemsAdded++;
			}
		}
	}
}

// Display the random list of items
function displayRandomCluster(rowId){
	var noItems = getNoOfPagesOrRows(randoms.length);
	var stop = display.numberDisplayedItems;

	if (randoms.length < stop) {
		stop = randoms.length;
	}

	$("#randoms").append(
		$.el.div({'class':'row',
				  'id':'thumbnailRandomRow' + rowId})
	);

	// populate page of random
	for (var index = 0; index < stop; index++){
			var id = getId(randoms[index].uri);

			$("#thumbnailRandomRow" + rowId).append(thumbnail(randoms[index]));
			addRandomClickEvent(id, randoms[index].link, rowId, index);
	}
}

// Add click events for random thumbnail items
function addRandomClickEvent(id, link, rowId, index) {
	// Add thumbnail click event
	$("#thumbnailRandomRow" + rowId  + " #" + id).click(function() {
		//Add info to local storage to be able to save context
		localStorage.setItem("itemIndex", index);
		localStorage.setItem("row", rowId);
		//localStorage.setItem("currentRandom", JSON.stringify(randoms[randomId]));
		document.location.href = link;
	});
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
function controls(labels) {
	if(display.showControls) {
		$("#resultsDiv").prepend(
			$.el.div({'class':'row'},
				$.el.div({'class':'col-md-12 resultsDivControls'}))
		);
		resultLayoutButtons(labels);
	}
}

// Add the buttons and the click functionality for changing the display
function resultLayoutButtons(labels) {
	$(".resultsDivControls").append(
		$.el.div({'class':'btn-group'},
			$.el.button({'class':'btn btn-default',
						 'id':'resultsBtnLayout'}))
	);
	setLayoutButton(labels);
	$("#resultsBtnLayout").click(function() {
		$("#resultsDiv").children().remove(".row");
		display.layout = (display.layout === "list") ? "cluster" : "list";
		controls(labels);
		displayView(labels);
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

/*******************************************************************************
View
Code for rendering either the cluster or the list view
*******************************************************************************/

// Displaying of the results based on the view chosen
// This function uses the data structures (clusters and randoms) that were
// populated before and just changes the way they are shown for user queries and
// recommendations based on user expertise. The random objects only get displayed
// as a list, so the button that selects the view (cluster or list) is not available
function displayView(labels){
	// TODO what if the result population or enrichment is not finished when
	// the user clicks the button? Maybe show the button after all this is finished?!

	// clear results div and display controls for changing the view again
	$("#resultsDiv").children().remove(".well, .well-sm");

	if(localStorage.query === "expertise") {
		statusMessage(labels.resultsHdrRecommendedResults);
	} else {
		// set page title and text for results header
		statusMessage(labels.resultsHdrResults + localStorage.query);
	}

	// display list view
	if(display.layout === "list") {
		// list view for user query
		var itemsAdded = 0;
		var totItems = totalItemsInClusters();

		// add rows for every cluster item
		addRows(totItems);

		for(var clusterId = 0; clusterId < clusters.length; clusterId++) {
			itemsAdded = displayList(clusterId, itemsAdded);
		}

		// list view for recommendation
		if(localStorage.query === "expertise") {
			statusMessage(labels.resultsHdrRandomResults);

			$(document).prop('title', labels.resultsTxtRecommendationsFor + labels.realName);

			var noRandomItems = randoms.length;

			// add rows for random objects and display them as a list
			addRandomRows(noRandomItems);

			displayRandomList();
		}
	// display cluster view
	} else if (display.layout === "cluster"){
		// cluster view for user query and recommendation
		for(var clusterId = 0; clusterId < clusters.length; clusterId++){
			displayClusterHeader(clusterId);
			displayClusterItems(clusterId);
		}

		// show random results for cluster view for recommendation
		if(localStorage.query === "expertise") {
			statusMessage(labels.resultsHdrRandomResults);

			$(document).prop('title', labels.resultsTxtRecommendationsFor + labels.realName);

			var noRandomItems = randoms.length;

			addRandomPath();

			//display items using pagination
			$("#randoms").append(pagination(getNoOfPagesOrRows(noRandomItems),
					randoms, "randoms"));
			displayRandomCluster(0);
		}
	}
}
