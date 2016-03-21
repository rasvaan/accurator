/******************************************************************************
Thumbnail

Code for initializing bootstrap thumbnails and handling changes
*******************************************************************************/

// Add thumbnails for a cluster
function thumbnails(clusterId) {
	var items = clusters[clusterId].items;
	var stop = display.numberDisplayedItems;
	var bootstrapWidth = parseInt(12/display.numberDisplayedItems, 10);

	// check if less results available then there are to be displayed
	if(items.length < stop){
		stop = items.length;
	}

	// add row
	$("#cluster" + clusterId).append(
		$.el.div({'class':'row', 'id':'thumbnailRow' + clusterId}));

	for (var i = 0; i < stop; i++) {
		id = getId(items[i].uri);
		$("#thumbnailRow" + clusterId).append(thumbnail(items[i]));
		addClickEvent(id, items[i].link, clusterId, i);
	}
}

// Retrieves the item id from the uri string
// Allow characters in the uri that won't trip up jQuery
function getId(uri) {
	var dirtyId = uri.substr(uri.lastIndexOf('/') + 1);
	var id = dirtyId.replace(/[^\w\-]/gi, '');
	return id;
}

// Generate HTML for adding a thumbnail for an item
function thumbnail(item) {
	var id = getId(item.uri);
	var bootstrapWidth = parseInt(12/display.numberDisplayedItems, 10);

	return $.el.div({'class':'col-md-' + bootstrapWidth},
				$.el.div({'class':'thumbnail',
						  'id':id},
					$.el.img({'src':item.thumb,
							  'class':'img-responsive',
							  'alt':''}),
						$.el.div({'class':'caption'},
							 thumbnailTitle(item, bootstrapWidth))));
}

// Retrieves the title of an item and sizes it accordingly
function thumbnailTitle(item, bootstrapWidth) {
	//Make header depent on size thumbnail
	if(bootstrapWidth < 4)
		return $.el.h5(item.title);
	if(bootstrapWidth >= 4)
		return $.el.h4(item.title);
}

// Add thumbnail click event
function addClickEvent(id, link, clusterId, index) {
	$("#cluster" + clusterId  + " #" + id).click(function() {
		//Add info to local storage to be able to save context
		localStorage.setItem("itemIndex", index);
		localStorage.setItem("clusterId", clusterId);
		localStorage.setItem("currentCluster", JSON.stringify(clusters[clusterId]));
		//TODO check here
		// if((clusterId+1) == clusters.length)
		// 	localStorage.setItem("query", "random");
		document.location.href = link;
	});
}

function changeThumbnails(pageNumber, activePage, numberOfPages, items, labelItems, clusterId) {
	var bootstrapWidth = parseInt(12/display.numberDisplayedItems, 10);
	//var items = clusters[clusterId].items;
	var start = (pageNumber - 1) * display.numberDisplayedItems;
	var stop = start + display.numberDisplayedItems;
	var remove = 0;
	var headerType;

	// Check if there are more spaces then items, if so, make those spaces invisible
	if(stop > items.length){
		remove = stop - items.length;
		stop = items.length;
		// console.log("Should make " + remove + " invisible.");
	}

	// console.log("start: " + start + " stop: " + stop + " page number: " + pageNumber + " current page: " + activePage + " cluster id: " + clusterId + " displayed: " + display.numberDisplayedItems + " remove: " + remove);
	var thumbIndex = 0;
	for (var i=start; i<stop; i++) {
		// console.log("Replacing thumb", thumbIndex);
		// Replace image
		// $("#" + labelItems + clusterId + " img").eq(thumbIndex).replaceWith(
		$("#" + labelItems + " img").eq(thumbIndex).replaceWith(
			$.el.img({'src':items[i].thumb,
					  'class':'img-responsive',
					  'alt':''}));
		// Replace title
		if(bootstrapWidth < 4)
			headerType = "h5";
		if(bootstrapWidth >= 4)
			headerType = "h4";

		// $("#" + labelItems + clusterId + " .caption " + headerType).eq(thumbIndex).replaceWith(
		$("#" + labelItems + " .caption " + headerType).eq(thumbIndex).replaceWith(
				thumbnailTitle(items[i], bootstrapWidth));

		// Replace id element and add new listener
		id = getId(items[i].uri)
		// $("#" + labelItems + clusterId + " .thumbnail").eq(thumbIndex).attr("id", id);
		$("#" + labelItems + " .thumbnail").eq(thumbIndex).attr("id", id);
		addClickEvent(id, items[i].link, clusterId, i);
		thumbIndex++;
	}

	// If returning from a possible invisible situation, make everything visible again
	if(activePage == numberOfPages) {
		var removed = numberOfPages * display.numberDisplayedItems - items.length;
		// console.log("Make " + removed + " thumbnail(s) visible again.");
		var start = display.numberDisplayedItems - removed;
		for(var i = start;i < display.numberDisplayedItems; i++) {
			// $("#" + labelItems + clusterId + " .col-md-" + bootstrapWidth).eq(i).show();
			$("#" + labelItems + " .col-md-" + bootstrapWidth).eq(i).show();
		}
	}

	// Don't display unused thumbspace
	for (var i = thumbIndex; i < thumbIndex+remove; i++) {
		// Remove slow?
		// $("#" + labelItems + clusterId + " .col-md-" + bootstrapWidth).eq(i).hide();
		$("#" + labelItems + " .col-md-" + bootstrapWidth).eq(i).hide();
	}
}
