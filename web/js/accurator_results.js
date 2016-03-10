/*******************************************************************************
Accurator Results

Page showing overview of recommender/search results. Uses a lot of code from
pagination.js and thumbnail.js.

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

var locale, domain, experiment, ui, user, userName, realName;
var resultsTxtRecommendationsFor, resultsTxtSearching, resultsHdrResults;
var resultsHdrFirst, resultsTxtFirst, resultsTxtNoResults, resultsTxtError;
var resultsHdrRecommendedResults, resultsHdrRandomResults;
var resultsLblCluster, resultsLblList;
var clusters = [];
var randoms = [];

// Display options deciding how to results get rendered
var display = {
	layout: "list",
	imageFilter: "onlyImages",
	numberDisplayedItems: 4,
	showControls: true
}

// Initialize page
function resultsInit() {
	locale = getLocale();
	domain = getDomain();
	experiment = getExperiment();
	populateFlags(locale);

	var onLoggedIn = function(loginData){
		setLinkLogo("profile");
		user = loginData.user;
		userName = getUserName(user);
		realName = loginData.real_name;
		var userQuery = getParameterByName("user");
		var query = getParameterByName("query");

		populateNavbar(userName, [{link:"profile.html",	name:"Profile"}]);

		var onDomain = function(domainData) {
			ui = domainData.ui + "results";
			var target = domainData.target;

			getLabels()
			.then(function(labels){
				initLabels(labels);
				events();
				addButtonEvents();

				// Provide results based on query, recommend something based on
				// the expertise of the retrieved user or, if none of these, show
				// just random results
				results(query, userQuery, target);
			});
		};
		domainSettings(domain, onDomain);
	};
	var onDismissal = function(){document.location.href = "intro.html";};
	logUserIn(onLoggedIn, onDismissal);
}

// Retrieve label elements
function getLabels() {
	return $.getJSON("ui_elements", {locale:locale, ui:ui,
							  		 type:"labels"});
}

// Add retrieved labels to html elements
function initLabels(labels) {
	$("#navbarBtnSearch").append(labels.navbarBtnSearch);
	$("#navbarBtnRecommend").append(labels.resultsBtnRecommend);
	resultsTxtRecommendationsFor = labels.resultsTxtRecommendationsFor;
	resultsTxtSearching = labels.resultsTxtSearching;
	resultsHdrResults = labels.resultsHdrResults;
	resultsHdrRecommendedResults = labels.resultsHdrRecommendedResults;
	resultsHdrRandomResults = labels.resultsHdrRandomResults;
	resultsHdrFirst = labels.resultsHdrFirst;
	resultsTxtFirst = labels.resultsTxtFirst;
	resultsTxtNoResults = labels.resultsTxtNoResults;
	resultsTxtError = labels.resultsTxtError;
	resultsLblCluster = labels.resultsLblCluster;
	resultsLblList = labels.resultsLblList;
}

// Add button events in the navbar
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

// Message displayed when the first annotation is made by a user
function events() {
	$.getJSON("annotations", {uri:user, type:"user"})
	.done(function(annotations){
		if(annotations.length === 0)
			alertMessage(resultsHdrFirst, resultsTxtFirst, 'success');
	});
}

// Add a title for the page and print a status message within the page that
// gives more information on the progress of the search
function statusMessage(header, text){
	//$("#resultsDiv").children().remove();
	//$(".col-md-10").empty();
	$(document).prop('title', header);

	$("#resultsDiv").append(
		$.el.div({'class':'row'},
			$.el.div({'class':'col-lg-10 col-md-offset-1'},
				$.el.h3(header)),
			$.el.div({'class':'row'},
				$.el.div({'class':'col-md-10 col-md-offset-1'},
					text)))
	);
}

/*******************************************************************************
Search, Recommend or Random results
*******************************************************************************/
function results(query, userQuery, target) {
	// Determine whether to recommend or give random results and set layout
	var recommendBoolean = recommenderExperiment();

	if(query) {
		// results based on the user query
		search(query);
	} else if(recommendBoolean) {
		// recommendations based on the expertise of the user
		// first recommended results are shown, then random results
		query = "expertise";
		recommend(query, target);
	} else {
		// random results
		query = "random";
		random(query, target, 10);
	}
	localStorage.setItem("query", query);
}

// Get results based on the user query
function search(query, target) {
	var request = {query:query};

	if(typeof target != 'undefined')
		request.target = target;

	$.getJSON("cluster_search_api", request)
	.then(function(data){
		// retrieve clusters
		clusters = data.clusters;

		// enrich retrieved clusters if any
		if(clusters.length == 0){
			statusMessage(resultsTxtNoResults, query);
		} else {
			// set page title and text for results header
			statusMessage(resultsHdrResults + query);

			// enrich the retrieved clusters
			enrichClusters(query);

			// Add control buttons to change layout
			controls();
		}
	})
	.fail(function(data){
		statusMessage(resultsTxtError, data.responseText);
	});
}

// Get results based on the expertise of the user and, afterwards, a number of
// random items that have not yet been annotated
function recommend(query, target) {
	$.getJSON("recommendation", {strategy:query,
								 target:target})
	.then(function(data){
		// retrieve clusters
		clusters = data.clusters;
		//localStorage.setItem("clusters", JSON.stringify(clusters));

		// enrich retrieved clusters if any
		if(clusters.length == 0){
			statusMessage(resultsTxtNoResults, query);
		} else {
			statusMessage(resultsHdrRecommendedResults);

			// enrich the retrieved clusters
			enrichClusters(query);
		}

		// populate random elements in the previously added random cluster
		random(query, target, 10);

		// set page title
		$(document).prop('title', resultsTxtRecommendationsFor + realName);

		// Add control buttons to change layout
		controls();
	})
	.fail(function(data){
		statusMessage(resultsTxtError, data.responseText)
	});
}

// Get random items
function random(query, target, noResults) {
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
			statusMessage(resultsTxtNoResults, query);
		} else {
			statusMessage(resultsHdrRandomResults);

			if (query === "expertise"){
				// set page title
				$(document).prop('title', resultsTxtRecommendationsFor + realName);
			}

			var noRandomItems = randoms.length;

			if (query === "expertise" && display.layout === "cluster"){
				addRandomPath();
				// TODO add pagination here for cluster items and add cluster items
				addRandomNodes(noRandomItems);
			} else {
				// add rows for random objects and display them as a list
				addRandomRows(noRandomItems);
			}

			// enrich random objects
			enrichRandoms(randoms)
			.then(function(){
				renderRandomList();
			});
		}
	})
	.fail(function(data){
		statusMessage(resultsTxtError, data.responseText)
	});
}

/*******************************************************************************
Result population
*******************************************************************************/

function enrichClusters(query) {
	// clear results div and reset rows
	//$("#resultsDiv").children().remove();

	// enrich retrieved clusters if any
	if(clusters.length != 0){ //double verification here (see previous function)
		// set page title
		$(document).prop('title', resultsHdrResults + query);

		// if the display is the list view, the rows that are needed can be first
		// created and after the enrichment is done, these can be further populated
		if (display.layout === "list") {
			var totItems = totalItemsInClusters();

			// add rows for every cluster item
			addRows(totItems);
		}
		var itemsAdded = 0;

		//for every cluster item
		for(var i = 0; i < clusters.length; i++) {
			if(display.layout === "cluster") {
				renderClusterHeader(i);
			}
			var uris = [];

			// enrich every item in the cluster
			for(var j = 0; j < clusters[i].items.length; j++) {
				uris[j] = clusters[i].items[j].uri;
			}

			//when a cluster item finished being enriched, display it
			enrichCluster(uris, i)
			.then(function (clusterId){
	  		  	// add enriched clusters and pagination
				if (display.layout === "cluster"){
					renderClusterItems(clusterId);
				// add enriched clusters and rows
				} else if (display.layout === "list"){
					itemsAdded = renderList(clusterId, itemsAdded);
				}
			});
		}
	}
}

// Add rows for cluster items for the list view
function addRows(totItems){
	var noRows = determineNumberOfPages(totItems);

	for (var i = 0; i < noRows; i++){
		$("#resultsDiv").append(
				$.el.div({'class':'row',
						 'id':'thumbnailRow' + i})
		);
	}
}

// Enrichment of one cluster item
function enrichCluster(uris, clusterId){
	var json = {"uris":uris};

	return $.ajax({type: "POST",
			url: "metadata",
			contentType: "application/json",
			data: JSON.stringify(json)})
	.then(function(data) {
		   // replace cluster items array with enriched ones
		   clusters[clusterId].items = processEnrichment(data);
		   return clusterId;
	 });
}

// Enrich one image element in the cluster adding an image, a link where it can
// be (further) annotated and a title
function processEnrichment(data) {
	//console.log("data=", data);
	var enrichedItems = [];

	for(var i=0; i<data.length; i++) {
		enrichedItems[i] = {};
		var uri = data[i].uri;
		enrichedItems[i].uri = uri;
		enrichedItems[i].thumb = data[i].thumb;
		enrichedItems[i].link = "annotate.html?uri=" + uri;
		enrichedItems[i].title = truncate(data[i].title, 60);
	}
	return enrichedItems;
}

// Determine the total number of items from clusters
function totalItemsInClusters(){
	var totItems = 0;

	for (var clusterId = 0; clusterId < clusters.length; clusterId++){
		totItems += clusters[clusterId].results;
	}
	return totItems;
}

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

// Determine number of pages or rows based on the items to be shown
// TODO maybe change the name of this function to better reflect its functionality
function determineNumberOfPages(numberOfItems) {
	var numberOfPages = 0;
	var restPages = numberOfItems%display.numberDisplayedItems;

	if(restPages == 0) {
		numberOfPages = numberOfItems/display.numberDisplayedItems;
	} else {
		numberOfPages = (numberOfItems-restPages)/display.numberDisplayedItems+1;
	}
	return numberOfPages;
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

// Add rows for random items
function addRandomRows(totItems){
	var noRows = determineNumberOfPages(totItems);

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
	var noRows = determineNumberOfPages(totItems);

	// add rows for thumbnails
    for (var i = 0; i < noRows; i++){
		// display as a list
		$("#randoms").append(
			$.el.div({'class':'row',
             		  'id':'thumbnailRandomRow' + i})
		);
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

/*******************************************************************************
Controls
Code for adding buttons controlling the layout
*******************************************************************************/
function controls() {
	if(display.showControls) {
		$("#resultsDiv").prepend(
			$.el.div({'class':'row'},
				$.el.div({'class':'col-md-12 resultsDivControls'}))
		);
		resultLayoutButtons();
	}
}

function resultLayoutButtons() {
	$(".resultsDivControls").append(
		$.el.div({'class':'btn-group'},
			$.el.button({'class':'btn btn-default',
						 'id':'resultsBtnLayout'}))
	);
	setLayoutButton();
	$("#resultsBtnLayout").click(function() {
		$("#resultsDiv").children().remove(".row");
		display.layout = (display.layout === "list") ? "cluster" : "list";
		controls();
		renderView();
	});
}

function setLayoutButton() {
	if(display.layout === "list") {
		$("#resultsBtnLayout").html(
			$.el.span(resultsLblCluster + ' ',
			$.el.span({'class':'glyphicon glyphicon-th-large'}))
		);
	} else {
		$("#resultsBtnLayout").html(
			$.el.span(resultsLblList + ' ',
			$.el.span({'class':'glyphicon glyphicon-th-large'}))
		);
	}
}

// Rendering of the results based on the view chosen
// This function uses the data structures (clusters and randoms) that were
// populated before and just changes the way they are shown for user queries and
// recommendations based on user expertise
// The random objects only get rendered as a list, so the button that selects the
// view (cluster or list) is not available
function renderView(){
	// TODO do not remove the view control buttons as well!!!
	// TODO what if the result population or enrichment is not finished when
	// the user clicks the button? Maybe show the button after all this is finished?!
	// clear results div and display controls for changing the view again
	$("#resultsDiv").children().remove(".well, .well-sm");

	// TODO this check first!
	// if(localStorage !== undefined){}

	// TODO add check for data loading: length, undefined, etc.

	if(localStorage.query === "expertise") {
		statusMessage(resultsHdrRecommendedResults);
	} else {
		// set page title and text for results header
		statusMessage(resultsHdrResults + localStorage.query);
	}

	// render list view
	if(display.layout === "list") {
		// list view for user query
		var itemsAdded = 0;
		var totItems = totalItemsInClusters();

		// add rows for every cluster item
		addRows(totItems);

		for(var clusterId = 0; clusterId < clusters.length; clusterId++) {
			itemsAdded = renderList(clusterId, itemsAdded);
		}

		// list view for recommendation
		if(localStorage.query === "expertise") {
			statusMessage(resultsHdrRandomResults);

			$(document).prop('title', resultsTxtRecommendationsFor + realName);

			var noRandomItems = randoms.length;

			// add rows for random objects and display them as a list
			addRandomRows(noRandomItems);

			renderRandomList();
		}
	// render cluster view
	} else if (display.layout === "cluster"){
		// cluster view for user query and recommendation
		for(var clusterId = 0; clusterId < clusters.length; clusterId++){
			renderClusterHeader(clusterId);
			renderClusterItems(clusterId);
		}

		// show random results for cluster view for recommendation
		if(localStorage.query === "expertise") {
			statusMessage(resultsHdrRandomResults);

			$(document).prop('title', resultsTxtRecommendationsFor + realName);

			var noRandomItems = randoms.length;

			addRandomPath();
			// TODO add pagination here for cluster items and add cluster items
			addRandomNodes(noRandomItems);

			renderRandomList();
		}
	}
}

function renderList(clusterId, itemsAdded){
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

function renderRandomList(){
	var noRows = determineNumberOfPages(randoms.length);
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

function renderClusterHeader(clusterId) {
	$("#resultsDiv").append(
		$.el.div({'class':'well well-sm',
				  'id':'cluster' + clusterId})
	);
	addPath(clusterId, clusters[clusterId].path, localStorage.query);
}

function renderClusterItems(clusterId){
	var noPages = determineNumberOfPages(clusters[clusterId].items.length);

	$("#cluster" + clusterId).append(pagination(noPages, clusterId));
	thumbnails(clusterId);
}
