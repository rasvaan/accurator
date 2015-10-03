/*******************************************************************************
Accurator Results
Page showing overview of recommender/search results. Uses a lot of code from
cluster_search_ui (search.js pagination.js and thumbnail.js)
*******************************************************************************/
var locale, experiment, ui, userName, realName;
var txtRecTitle, vntFirstTitle, vntFirstText;

// Options provided for cluster_search_ui__search.js
displayOptions = {
	numberDisplayedItems: 4,
	showFilters: false,
	imageFilter: 'onlyImages',
	//Indicate whether the result link points to annotation view or regular result view
	annotateLink: true
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

	$("#btnResultsSearch").append(labels.btnResultsSearch);
	$("#btnResultsRecommend").append(labels.btnResultsRecommend);
	txtRecTitle = labels.txtRecTitle;
	vntFirstTitle = labels.vntFirstTitle;
	vntFirstText = labels.vntFirstText;
}

function addButtonEvents() {
	$("#btnResultsRecommend").click(function() {
		document.location.href="results.html" + "?user=" + user;
	});
	// Search on pressing enter
	$("#frmSearch").keypress(function(event) {
		if (event.which == 13) {
			var query = encodeURIComponent($("#frmSearch").val());
			document.location.href="results.html?query=" + query;
		}
	});
	$("#btnResultsSearch").click(function() {
		var query = encodeURIComponent($("#frmSearch").val());
		document.location.href="results.html?query=" + query;
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

function initiateSearch(query, target) {
	search(query, target);
}

function recommendItems(target) {
	if(experiment === "recommender") {
		// If running an recommender experiment choose A or B
		randomOrRecommended(target);
	} else {
		// Business as usual
		recommendExpertiseItems(target);
	}
}

function recommendExpertiseItems(target) {
	query = "expertise values";
	$.getJSON("recommendation", {strategy:'expertise',
								 target:target})
	.done(function(data){
		setGlobalQuery(query)
		$("#results").children().remove();
		showFilters();
		processJsonResults(data);
		createResultClusters();
		$(document).prop('title', txtRecTitle + realName);
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
		recommendExpertiseList(query, target);
	} else if(AOrB === "random") {
		randomResults(query, target);
	}
}

function recommendExpertiseList(target) {
	query = "expertise";

	$.getJSON("recommendation", {strategy:'expertise',
								 number:20,
								 target:target,
							 	 output_format:'list'})
	.done(function(data){
		// setGlobalQuery(query)
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
	query = "random";

	// Populate a list of random items
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
	new Pengine({server: 'pengine',
				 application: 'enrichment',
				 ask: 'maplist(enrich_item,' + Pengine.stringify(itemUris, {string:'atom'}) + ', Items),!',
				 onsuccess: function () {
					enrichedItems = processListEnrichment(this.data);
					thumbnailList(enrichedItems);
	}});
}

function processListEnrichment(data) {
	var sourceItems = data[0].Items;
	var numberOfItems = sourceItems.length;
	var items = [];

	// console.log("display", displayOptions.annotateLink);
	for (var i=0; i<numberOfItems; i++) {
		var uri = sourceItems[i].uri;
		var thumb = sourceItems[i].thumb;
		var link = "annotate_image.html?uri=" + uri;
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
