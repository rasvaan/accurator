/*******************************************************************************
Accurator Results
Page showing overview of recommender/search results. Uses a lot of code from
cluster_search_ui (search.js pagination.js and thumbnail.js)
*******************************************************************************/
var locale, experiment, ui, typeRec, userName, realName;
var resultsTxtRecommendationsFor, resultsHdrFirst, resultsTxtFirst;

// Options provided for search.js
displayOptions = {
	numberDisplayedItems: 4,
	showFilters: false,
	imageFilter: 'onlyImages'
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
		var userParam = getParameterByName("user");
		var query = getParameterByName("query");

		populateNavbar(userName, [{link:"profile.html",	name:"Profile"}]);

		onDomain = function(domainData) {
			ui = domainData.ui + "results";
			var target = domainData.target;
			populateUI();
			addButtonEvents();

			//Provide results based on query or recommend something. In case of no in put recommend based on retrieved user.
			if(query != "") {
				initiateSearch(query, target);
			} else {
				recommendItems(target);
				query = typeRec;
			}
			localStorage.setItem("query", query);
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

	$("#resultsBtnSearch").append(labels.resultsBtnSearch);
	$("#resultsBtnRecommend").append(labels.resultsBtnRecommend);
	resultsTxtRecommendationsFor = labels.resultsTxtRecommendationsFor;
	resultsHdrFirst = labels.resultsHdrFirst;
	resultsTxtFirst = labels.resultsTxtFirst;
}

function addButtonEvents() {
	$("#resultsBtnRecommend").click(function() {
		document.location.href="results.html" + "?user=" + user;
	});
	// Search on pressing enter
	$("#navbarInpSearch").keypress(function(event) {
		if (event.which == 13) {
			var query = encodeURIComponent($("#navbarInpSearch").val());
			document.location.href="results.html?query=" + query;
		}
	});
	$("#resultsBtnSearch").click(function() {
		var query = encodeURIComponent($("#navbarInpSearch").val());
		document.location.href="results.html?query=" + query;
	});
}

function events() {
	$.getJSON("annotations", {uri:user, type:"user"})
	.done(function(annotations){
		uris = annotations.uris;
		if(uris.length===0) {
			alertMessage(resultsHdrFirst, vntFirstText, 'success');
		}
	});
}

function initiateSearch(query, target) {
	search(query, target);
}

function recommendItems(target) {
	if(experiment === "recommender") {
		// If running an recommender experiment choose A or B
		randomOrRecommended(target);
	} else if (experiment === "random"){
		// Lets do random stuff
		randomItems(target);
	} else {
		// Business as usual
		recommendExpertiseItems(target);
	}
}

function recommendExpertiseItems(target) {
	typeRec = "expertise values";

	$.getJSON("recommendation", {strategy:'expertise',
								 target:target})
	.done(function(data){
		$("#results").children().remove();
		showFilters();
		processJsonResults(data);
		createResultClusters();
		$(document).prop('title', resultsTxtRecommendationsFor + realName);
		//Also get a row of random items not yet annotated
		populateRandom(target, data.clusters.length);
	})
	.fail(function(data, textStatus){
		$("#results").children().remove();
		$("#results").append(errorHtml(data, textStatus));
		$(document).prop('title', 'Error on ' + query);
	});
}

function populateRandom(target, clusterIndex) {
	$("#results").append(clusterContainer(clusterIndex));
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

function randomOrRecommended(target) {
	// Consider recommendation AB setting
	var AOrB = getAOrB();

	if(AOrB === "recommend") {
		recommendExpertiseList(target);
	} else if(AOrB === "random") {
		randomResults(target);
	}
}

function randomItems(target) {
	typeRec = "random";

	// Populate a list of random items
	$.getJSON("recommendation", {strategy:'random',
								 number:250,
								 target:target})
	.done(function(data){
		// Set retrieved uris as current cluster
		localStorage.setItem("currentCluster", JSON.stringify(data));
		var numberOfItems = data.length;
		var items = [];

		for (var i=0; i<numberOfItems; i++) {
			var uri = data[i];
			items[i] = new item(uri);
		}
		addItemList(items);
	});
}

function recommendExpertiseList(target) {
	typeRec = "expertise";

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
		$("#results").children().remove();
		$("#results").append(errorHtml(data, textStatus));
		$(document).prop('title', 'Error on ' + query);
	});
}

function randomResults(target) {
	typeRec = "random";

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

	// console.log("display", displayOptions.annotateLink);
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
		$("#results").append(
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
