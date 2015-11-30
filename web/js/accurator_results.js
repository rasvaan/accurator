/*******************************************************************************
Accurator Results
Page showing overview of recommender/search results. Uses a lot of code from
pagination.js and thumbnail.js
*******************************************************************************/
var query = "", locale, experiment, ui, userName, realName;
var resultsTxtRecommendationsFor, resultsHdrFirst, resultsTxtFirst;
var jsonResults, initialClusters = [], enrichedClusters = [], clusters = [];

// Display options deciding how to results get rendered
display = {
	layout: "cluster",
	imageFilter: "onlyImages",
	numberDisplayedItems: 4,
	showControls: true
}

function resultsInit() {
	locale = getLocale();
	domain = getDomain();
	experiment = getExperiment();

	populateFlags(locale);

	onLoggedIn = function(loginData){
		setLinkLogo("profile");
		user = loginData.user;
		userName = getUserName(user);
		realName = loginData.real_name;
		var userQuery = getParameterByName("user");
		var query = getParameterByName("query");
		populateNavbar(userName, [{link:"profile.html",	name:"Profile"}]);

		onDomain = function(domainData) {
			ui = domainData.ui + "results";
			var target = domainData.target;
			populateUI();
			addButtonEvents();

			//Provide results based on query or recommend something. In case of no in put recommend based on retrieved user.
			results(query, userQuery, target);
		};
		domainSettings(domain, onDomain);
	};
	onDismissal = function(){document.location.href="intro.html";};
	logUserIn(onLoggedIn, onDismissal);
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui,
							  type:"labels"})
	.done(function(labels){
		initLabels(labels);
		events();
	});
}

function initLabels(labels) {
	// Add retrieved labels to html elements
	document.title = labels.title;

	$("#navbarBtnSearch").append(labels.navbarBtnSearch);
	$("#navbarBtnRecommend").append(labels.resultsBtnRecommend);
	resultsTxtRecommendationsFor = labels.resultsTxtRecommendationsFor;
	resultsHdrFirst = labels.resultsHdrFirst;
	resultsTxtFirst = labels.resultsTxtFirst;
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

function events() {
	$.getJSON("annotations", {uri:user, type:"user"})
	.done(function(annotations){
		if(annotations.length===0)
			alertMessage(resultsHdrFirst, annotateTxtFirst, 'success');
	});
}

function results(query, userQuery, target) {
	// Determine whether to recommend or give random results and set layout
	var recommend = recommenderExperiment();

	if(query) {
		search(query);
	} else if(recommend) {
		recommendItems(userQuery, target);
		query = "expertise values";
	} else {
		randomItems(target);
		query = "random";
	}
	localStorage.setItem("query", query);
}

function search(query, target) {
	//TODO: localize variables
	var resultsTxtSearching = "Searching for ";
	var resultsHdrResults = "Results for ";
	var resultsTxtError = "Unfortunately an error has occured";

	var request = {query:query};
	if(typeof target != 'undefined') request.target = target;

	statusMessage(resultsTxtSearching + query);

	$.getJSON("cluster_search_api", request)
	.done(function(data){
		// Make available for future changes of view
		jsonResults = data;
		populateResults();
		// Change title of page
		$(document).prop('title', resultsHdrResults + query);
	})
	.fail(function(data){
		statusMessage(resultsTxtError, data.responseText)
	});
}

function populateResults() {
	// Clear results div
	$("#resultsDiv").children().remove();
	// Results layout is either cluster or list
	if(display.layout === "cluster") {
		populateClusters();
	} else if(display.layout === "list") {
		//TODO: flatten results clusters
		populateList();
	}
	// Add control buttons
	controls();
}

/*******************************************************************************
Cluster view
Show the results in clusters
*******************************************************************************/
function populateClusters() {
	console.log(jsonResults);
	jsonToClusters(jsonResults);
	createResultClusters();
}

function jsonToClusters(data) {
	console.log(data);
	// Convert json to initialClusters array
	var sourceClusters = data.clusters;
	var numberOfClusters = sourceClusters.length;

	for (var i=0;i<numberOfClusters;i++) {
		// Get path uris and query for the labels
		var path = sourceClusters[i].path;
		var numberOfItems = sourceClusters[i].results;
		var items = [];

		for (var j=0;j<numberOfItems;j++) {
			var uri = sourceClusters[i].items[j].uri;
			items[j] = new item(uri);
		}
		initialClusters[i] = new cluster(path, items);
	}
}

function createResultClusters() {
	if(initialClusters.length == 0){
		// console.log('No results found for ', query);
		$("#resultsDiv").append(noResultsHtml(query));
	} else {
		for(var i=0;i<initialClusters.length;i++) {
			$("#resultsDiv").append(clusterContainer(i));
			// Append path to cluster container
			addPath(i);
			// Add enriched clusters and pagination
			addItems(i);
		}
	}
}

/*******************************************************************************
List view
Show the results in one big list
*******************************************************************************/
function populateList() {
	console.log(jsonResults);
}

function recommendItems(target) {
	console.log("Recommending items");

	$.getJSON("recommendation", {strategy:'expertise',
								 target:target})
	.done(function(data){
		$("#resultsDiv").children().remove();
		showFilters();
		processJsonResults(data);
		createResultClusters();
		$(document).prop('title', resultsTxtRecommendationsFor + realName);
		//Also get a row of random items not yet annotated
		populateRandom(target, data.clusters.length);
	})
	.fail(function(data, textStatus){
		$("#resultsDiv").children().remove();
		$("#resultsDiv").append(errorHtml(data, textStatus));
		$(document).prop('title', 'Error on ' + query);
	});
}

function randomItems(target) {
	console.log("Providing random items");

	// Populate a list of random items
	$.getJSON("recommendation", {strategy:'random',
								 number:12,
								 target:target})
	.done(function(data){
		var numberOfItems = data.length;
		var items = [];

		for (var i=0; i<numberOfItems; i++) {
			var uri = data[i];
			items[i] = new item(uri);
		}
		addItemList(items);
	});
}

function populateRandom(target, clusterIndex) {
	$("#resultsDiv").append(clusterContainer(clusterIndex));
	$.getJSON("recommendation", {strategy:'random',
								 number:20,
								 target:target})
	.done(function(data){
		var numberOfItems = data.length;
		var items = [];

		for (var i=0; i<numberOfItems; i++) {
			var uri = data[i];
			items[i] = new item(uri);
		}
		initialClusters[clusterIndex] = new cluster([], items);
		enrichedClusters[clusterIndex] = new cluster([], 'undefined');
		$("#cluster"+clusterIndex).prepend(
			$.el.h4(
				$.el.span({'class':'path-label path-literal'},
					"random objects")));
		addItems(clusterIndex);
	});
}

function recommendExpertiseList(target) {
	console.log("Recommending list of items");

	$.getJSON("recommendation", {strategy:'expertise',
								 number:12,
								 target:target,
							 	 output_format:'list'})
	.done(function(data){
		var numberOfItems = data.length;
		var items = [];

		for (var i=0; i<numberOfItems; i++) {
			var uri = data[i];
			items[i] = new item(uri);
		}
		addItemList(items);
	})
	.fail(function(data, textStatus){
		$("#resultsDiv").children().remove();
		$("#resultsDiv").append(errorHtml(data, textStatus));
		$(document).prop('title', 'Error on ' + query);
	});
}

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
	});
}

function setLayoutButton() {
	//TODO: localize variables
	var resultsLblCluster = "Cluster view";
	var resultsLblList = "List view";

	if(display.layout === "list") {
		$("#resultsBtnLayout").html(
			$.el.span(resultsLblList + ' ',
			$.el.span({'class':'glyphicon glyphicon-th-large'}))
		);
	} else {
		$("#resultsBtnLayout").html(
			$.el.span(resultsLblCluster + ' ',
			$.el.span({'class':'glyphicon glyphicon-th-large'}))
		);
	}
}

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
Result List
*******************************************************************************/
function addItemList(items) {
	var itemUris = [];
	for(var i=0;i<items.length;i++)
		itemUris[i] = items[i].uri;

	// Get item enrichments from server, on success add pagination and thumbnails
	var json = {"uris":itemUris};
	$.ajax({type: "POST",
			url: "metadata",
			contentType: "application/json",
			data: JSON.stringify(json),
			success: function(data) {
				enrichedItems = processListEnrichment(data);
				thumbnailList(enrichedItems);
		   }
	});
}

function processListEnrichment(sourceItems) {
	var numberOfItems = sourceItems.length;
	var items = [];

	// console.log("display", display.annotateLink);
	for (var i=0; i<numberOfItems; i++) {
		var uri = sourceItems[i].uri;
		var thumb = sourceItems[i].thumb;
		//var link = "annotate_image.html?uri=" + uri;
		var link = "annotate.html?uri=" + uri;
 		var title = truncate(sourceItems[i].title, 60);
		items[i] = new item(uri, thumb, link, title);
	}
	return items;
}

function thumbnailList(items) {
	var rowLength = 4;
	bootstrapWidth = parseInt(12/rowLength, 10);
	var numberOfRows = items.length/rowLength;
	var itemsAdded = 0;

	for(var i=1; i<=numberOfRows; i++) {
		// Add row for thumbnails
		$("#resultsDiv").append(
			$.el.div({'class':'row', 'id':'thumbnailRow' + i}));

		//Determine where to stop adding
		var stop = i * rowLength;
		if(items.length<stop){
			stop = items.length;
		}

		for (var j=itemsAdded; j<stop; j++) {
			id = getId(items[j].uri);

			$("#thumbnailRow" + i).append(
				$.el.div({'class':'col-md-' + bootstrapWidth},
					$.el.div({'class':'thumbnail',
							  'id':id},
						$.el.img({'src':items[j].thumb,
						          'class':'img-responsive',
								  'alt':''}),
							$.el.div({'class':'caption'},
								 thumbnailTitle(j, items, bootstrapWidth))))
			);
			addListClickEvent(id, items[j].link, i, j);
			itemsAdded++;
		}
	}
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
