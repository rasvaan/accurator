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

var rows = 0, locale, domain, experiment, ui, user, userName, realName;
var resultsTxtRecommendationsFor, resultsTxtSearching, resultsHdrResults;
var resultsHdrFirst, resultsTxtFirst, resultsTxtNoResults, resultsTxtError;
var resultsLblCluster, resultsLblList;
var clusters = [];

// Display options deciding how to results get rendered
var display = {
	layout: "cluster",
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

			//partial fix
			// $.when(populateUI()).then(function (){
			// 	addButtonEvents();
			// 	results(query, userQuery, target);
			// });

			populateUI(query, userQuery, target);

			// populateUI();
			addButtonEvents();

			// Provide results based on query, recommend something based on
			// the expertise of the retrieved user or, if none of these, show
			// just random results
			//results(query, userQuery, target);
		};
		domainSettings(domain, onDomain);
	};
	var onDismissal = function(){document.location.href = "intro.html";};
	logUserIn(onLoggedIn, onDismissal);
}

// Retrieve label elements
function populateUI(query, userQuery, target) {
//function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui,
							  type:"labels"})
	.done(function(labels){
		initLabels(labels);
		events();
		results(query, userQuery, target);
	});
}

// Add retrieved labels to html elements
function initLabels(labels) {
	$("#navbarBtnSearch").append(labels.navbarBtnSearch);
	$("#navbarBtnRecommend").append(labels.resultsBtnRecommend);
	resultsTxtRecommendationsFor = labels.resultsTxtRecommendationsFor;
	resultsTxtSearching = labels.resultsTxtSearching;
	resultsHdrResults = labels.resultsHdrResults;
	resultsHdrFirst = labels.resultsHdrFirst;
	resultsTxtFirst = labels.resultsTxtFirst;
	resultsTxtNoResults = labels.resultsTxtNoResults;
	resultsTxtError = labels.resultsTxtError;
	resultsLblCluster = labels.resultsLblCluster;
	resultsLblList = labels.resultsLblList;
	console.log("retrieved labels, ex. resultsBtnRecommend=", labels.resultsBtnRecommend);
	console.log("retrieved labels, ex. resultsTxtSearching=", resultsTxtSearching);
	console.log("retrieved labels, ex. resultsTxtNoResults=", resultsTxtNoResults);
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
		var uris = annotations.uris;

		if (uris.length===0) {
			alertMessage(resultsHdrFirst, resultsTxtFirst, 'success');
		}
	});
}

// Add a title for the page and print a status message within the page that
// gives more information on the progress of the search
function statusMessage(header, text){
	$("#resultsDiv").children().remove();
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
		query = "expertise values";
		recommend(userQuery, target);
	} else {
		//random results
		query = "random";
		random(target);
	}
	localStorage.setItem("query", query);
}

// Get results based on the user query
function search(query, target) {
	var request = {query:query};
	if(typeof target != 'undefined') request.target = target;

	console.log("query", query);
	console.log("resultsTxtSearching", resultsTxtSearching);

	if (! resultsTxtSearching === undefined) {
		statusMessage(resultsTxtSearching + query);
	}
	else
		statusMessage("undefined resultsTxtSearching" + query);

	$.getJSON("cluster_search_api", request)
	.done(function(data){
		// retrieve clusters
		clusters = data.clusters;
		// populate the page with the cluster and their items
		populateResults(query);
		console.log("resultsHdrResults", resultsHdrResults);
		$(document).prop('title', resultsHdrResults + query);
	})
	.fail(function(data){
		statusMessage(resultsTxtError, data.responseText);
	});
}

// Get results based on the expertise of the user and, afterwards, a number of
// random items that have not yet been annotated
function recommend(userQuery, target) {
	//TODO: add userQuery as variable
	$.getJSON("recommendation", {strategy:'expertise',
								 target:target})
	.done(function(data){
		clusters = data.clusters;
		populateResults("expertise");
		$(document).prop('title', resultsTxtRecommendationsFor + realName);
		// Get a number of random items not yet annotated
		populateRandom(target, clusters.length);
	})
	.fail(function(data){
		statusMessage(resultsTxtError, data.responseText)
	});
}

// function recommendExpertiseList(target) {
// 	console.log("Recommending list of items");
//
// 	$.getJSON("recommendation", {strategy:'expertise',
// 								 number:12,
// 								 target:target,
// 							 	 output_format:'list'})
// 	.done(function(data){
// 		var numberOfItems = data.length;
// 		var items = [];
//
// 		for (var i=0; i<numberOfItems; i++) {
// 			var uri = data[i];
// 			items[i] = new item(uri);
// 		}
// 		addItemList(items);
// 	})
// 	.fail(function(data, textStatus){
// 		$("#resultsDiv").children().remove();
// 		$("#resultsDiv").append(errorHtml(data, textStatus));
// 		$(document).prop('title', 'Error on ' + query);
// 	});
// }

// Get random items
function random(target) {
	//var resultList = new ResultList();
	// Get a list of random items
	$.getJSON("recommendation", {strategy:'random',
								 number:10,
								 target:target})
	.done(function(uris){
		// Always populate a list if random
		populateListItems(uris, resultList);
	});
}

/*******************************************************************************
Result population
*******************************************************************************/
function populateResults(query) {
	// Clear results div and reset rows
	$("#resultsDiv").children().remove();
	rows = 0; //check here; even if it is done at the start of the page, otherwise it does not render all items in list view

	// Results layout is either cluster or list
	if(display.layout === "cluster") {
		populateClusters(query);
	} else if(display.layout === "list") {
		var resultList = new ResultList();
		populateList(clusters, resultList);
	}
	// Add control buttons
	controls();
}

function populateList(clusters, resultList){
	var index = 0;

	for(var i=0; i<clusters.length; i++) {
		var uris = [];

		for(var j=0; j<clusters[i].items.length; j++)
			uris[j] = clusters[i].items[j].uri;
		populateListItems(uris, resultList);
	}
}

function populateRandom(target, clusterIndex) {
	$.getJSON("recommendation", {strategy:'random',
								 number:20,
								 target:target})
	.done(function(uris){
		$("#cluster"+clusterIndex).prepend(
			$.el.h4(
				$.el.span({'class':'path-label path-literal'},
					"random objects")));
		addItems(uris);
	});

	$("#resultsDiv").append(
		$.el.div({'class':'well well-sm',
				  'id':'cluster' + clusterIndex})
	);
}

/*******************************************************************************
List view
Show the results in one big list
*******************************************************************************/
function populateListItems(uris, resultList) {
	// Determine indexes for adding elements in items (important since metadata
	// call is asynchronous)
	var start = resultList.length;
	resultList.addUris(uris);
	var stop = resultList.length;

	// Get metedata for uris
	$.ajax({type: "POST",
			url: "metadata",
			contentType: "application/json",
			data: JSON.stringify({"uris":uris}),
			success: function(data) {
				// Add enriched items to items array and view
				var index = 0;

				for(var i=start; i<stop; i++) {
					resultList.addNthItem(
						data[index].uri,
						data[index].thumb,
						data[index].title,
						i
					);
					index++;
				}

				populateThumbnails(start, stop, resultList);
		   }
	});
}

function populateThumbnails(begin, end, resultList) {
	// Determine in which row to start adding thumbnails
	var rowLength = display.numberDisplayedItems;
	var startRows = parseInt(begin/rowLength, 10) + 1;
	var stopRows = parseInt(end/rowLength, 10) + 1;
	var itemsAdded = begin;
	var items = resultList.items;
	// See if additional rows need to be added
	addRows(stopRows);

	// Add the thumbnails to the rows
	for(var i=startRows; i<=stopRows; i++) {
		// Determine where to start adding
		var start = parseInt(itemsAdded/rowLength, 10);
		// Determine where to stop adding
		var stop = i * rowLength;
		if(end < stop) stop = end;

		for (var j=itemsAdded; j<stop; j++) {
			var id = getId(items[j].uri);
			$("#thumbnailRow" + i).append(thumbnail(items[j]));

			// addThumbnail(i, j, id, items[j], itemWidth);
			addListClickEvent(id, items[j].link, i, j);
			itemsAdded++;
		}
	}
}

function addRows(numberOfRows) {
	for(var i=rows+1; i<numberOfRows+1; i++) {
		// Add row for thumbnails
		$("#resultsDiv").append(
			$.el.div({'class':'row', 'id':'thumbnailRow' + i}));
	}
	rows = numberOfRows;
}

function addListClickEvent(id, link, rowId, index) {
	//Add thumbnail click event
	$("#thumbnailRow" + rowId  + " #" + id).click(function() {
		//Add info to local storage to be able to save context
		localStorage.setItem("itemIndex", index);
		localStorage.setItem("row", rowId);
		document.location.href=link;
	});
}

function ResultList() {
	//Object for keeping track of list of results
	this.uris = [];
	this.items = [];
	this.length = 0;

	this.addUris = function(newUris) {
		// Add items to uri list
		for(var i=0; i<newUris.length; i++)
			this.addUri(newUris[i]);
	}

	this.addUri = function(uri) {
		// Add uri
		this.uris[this.length] = uri;
		// Update length
		this.length++;
	}

	// Add item at nth index in the list, allows to keep order
	this.addNthItem = function(uri, thumb, title, index) {
		var item = {};
		item.uri = uri;
		item.thumb = thumb;
		item.link = "annotate.html?uri=" + uri;
		item.title = truncate(title, 60);
		this.items[index] = item;
	}
}

/*******************************************************************************
Cluster view
Show the results in clusters
*******************************************************************************/
function populateClusters(query) {
	var query = query || "literal";

	if(clusters.length == 0){
		statusMessage(resultsTxtNoResults, query);
	} else {
		for(var i=0; i<clusters.length; i++) {
			var cluster = clusters[i];
			var uris = [];

			for(var j=0; j<cluster.items.length; j++)
				uris[j] = cluster.items[j].uri;

			$("#resultsDiv").append(
				$.el.div({'class':'well well-sm',
						  'id':'cluster' + i})
			);

			addPath(i, cluster.path, query);
			// Add enriched clusters and pagination
			addItems(uris, i);
		}
	}
}

function addPath(clusterId, uris, query) {
	// Get labels from server
	var json = {"uris":uris, "type":"label"};
	$.ajax({type: "POST",
			url: "metadata",
			contentType: "application/json",
			data: JSON.stringify(json),
			success: function(labels) {
				var pathElements = [];

			    for(var i=0; i<uris.length; i++)
			        pathElements[i] = {uri:uris[i], label:truncate(labels[i], 50)};

			    pathElements.reverse();
				var path = new Path(uris, labels, pathElements);
				$("#cluster" + clusterId).prepend(path.htmlSimple);
				path.unfoldEvent("#cluster" + clusterId, query);
		   }
	});
}

function addItems(uris, clusterId) {
	var json = {"uris":uris};
	$.ajax({type: "POST",
			url: "metadata",
			contentType: "application/json",
			data: JSON.stringify(json),
			success: function(data) {
				// Replace cluster items with enriched items
				clusters[clusterId].items = processEnrichment(data);
				var pages = determineNumberOfPages(clusterId);
				$("#cluster"+clusterId).append(pagination(pages, clusterId));
				thumbnails(clusterId);
		   }
	});
}

function processEnrichment(data) {
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

function determineNumberOfPages (clusterId) {
	var numberOfPages = 0;
	var numberOfItems = clusters[clusterId].items.length;
	var restPages = numberOfItems%display.numberDisplayedItems;

	//Determine number of items in pagination
	if(restPages == 0) {
		numberOfPages = numberOfItems/display.numberDisplayedItems;
	} else {
		numberOfPages = (numberOfItems-restPages)/display.numberDisplayedItems+1;
	}
	return numberOfPages;
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
		display.layout = (display.layout === "list") ? "cluster" : "list";
		setLayoutButton();
		populateResults();
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
