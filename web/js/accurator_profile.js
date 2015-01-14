/* Accurator Profile
*/
var user = "rasvaan";
var locale = "en";
var ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#profile";
var recentItems;

server = {
		location: getServerUrl()
}

displayOptions = {
		numberDisplayedItems: 6,
}

function getServerUrl() {
	var urlParts = document.location.href.split("profile");
	return urlParts[0];
}

function profileInit() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
		.done(function(data){
			addButtonEvents();
			initLabels(data);
			getRecentlyAnnotated();})
		.fail(function(data, textStatus){
			$("#txtSubSlogan").replaceWith('Problem connecting to server, please contact the system administrator');});
}

function initLabels(data) {
	$("#txtSlogan").prepend(data.txtSlogan + " " + user);
	$("#txtSubSlogan").prepend(data.txtSubSlogan);
	$("#btnRecommend").append(data.btnRecommend);
	$("#btnChangeExpertise").append(data.btnChangeExpertise);
	$("#btnSearch").append(data.btnSearch);
	$("#lblLastAnnotated").append(data.lblLastAnnotated);
}

function addButtonEvents() {
	$("#btnRecommend").click(function() {
		document.location.href="/recommendations.html";
	});
	$("#btnChangeExpertise").click(function() {
		document.location.href="/expertise.html";
	});	
}

function getRecentlyAnnotated() {
	$.getJSON("recently_annotated", {user:user})
	.done(function(data){
			addItems(data);
		})
	.fail(function(data, textStatus){});
}

function addItems(data) {
	var itemUris = data.uris;
	// Get item enrichments from server, on success add pagination and thumbnails
	new Pengine({server: server.location + 'pengine',
				 application: 'enrichment',
				 ask: 'maplist(enrich_item,' + Pengine.stringify(itemUris, {string:'atom'}) + ', Items),!',
				 onsuccess: function () {
					clusterId = 0;
					recentItems = processEnrichment(this.data, clusterId);
					// Clone cluster to enable filtering without losing information.
					if(recentItems.length==0) {
						$("#cluster0").append(noResultsHtml());
					} else {
						var pages = determineNumberOfPages(recentItems.length);
						$("#cluster0").append(pagination(pages, clusterId));
						$("#cluster0").append(thumbnails(recentItems, clusterId));
					}
	}});
}

function noResultsHtml() {
	return $.el.h5('You should start annotating to see some results!');
}

function determineNumberOfPages(numberOfItems) {
	var numberOfPages = 0;
	var restPages = numberOfItems%displayOptions.numberDisplayedItems;
	
	//Determine number of items in pagination
	if(restPages == 0) {
		numberOfPages = numberOfItems/displayOptions.numberDisplayedItems;
	} else {
		numberOfPages = (numberOfItems-restPages)/displayOptions.numberDisplayedItems+1;
	}
	return numberOfPages;
}

function processEnrichment(data) {
	var sourceItems = data[0].Items;
	var numberOfItems = sourceItems.length;
	var items = [];
		
	for (var i=0; i<numberOfItems; i++) {
		var uri = sourceItems[i].uri;
		var thumb = sourceItems[i].thumb;
		var link = sourceItems[i].link;
		var title = truncate(sourceItems[i].title, 60);
		items[i] = new item(uri, thumb, link, title);
	}
	return items;
}

function item(uri, thumb, link, title) {
	this.uri = uri;
	this.thumb = thumb;
	this.link = link;
	this.title = title;
}

function truncate (string, limit) {
	var chars;
	var i;
	
	chars = string.split('');
	if (chars.length > limit) {
		for (i=chars.length - 1; i>-1; --i) {
			if (i>limit) {
				chars.length = i;
			}
			else if (' ' === chars[i]) {
				chars.length = i;
				break;
			}
		}
		chars.push('...');
	}
	return chars.join('');
}