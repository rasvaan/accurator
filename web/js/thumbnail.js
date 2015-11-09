/* Thumbnail
*  Code for initializing bootstrap thumbnails and handling changes
*/
var boodstrapWitdth;

function thumbnails(clusterId) {
	var items = clusters[clusterId].items;
	var stop = displayOptions.numberDisplayedItems;
	bootstrapWidth = parseInt(12/displayOptions.numberDisplayedItems, 10);

	//Check if less results available then there are to be displayed
	if(items.length<stop){
		stop = items.length;
	}
	//Add row
	$("#cluster"+clusterId).append(
		$.el.div({'class':'row', 'id':'thumbnailRow'+clusterId}));

	for (var i=0; i<stop; i++) {
		id = getId(items[i].uri);
		$("#thumbnailRow" + clusterId).append(thumbnail(items[i]));
		addClickEvent(id, items[i].link, clusterId, i);
	}
}

function thumbnail(item) {
	var bootstrapWidth = parseInt(12/displayOptions.numberDisplayedItems, 10);
	var id = getId(item.uri);

	return $.el.div({'class':'col-md-' + bootstrapWidth},
				$.el.div({'class':'thumbnail',
						  'id':id},
					$.el.img({'src':item.thumb,
							  'class':'img-responsive',
							  'alt':''}),
						$.el.div({'class':'caption'},
							 thumbnailTitle(item, bootstrapWidth))));
}

function thumbnailTitle(item, bootstrapWidth) {
	//Make header depent on size thumbnail
	if(bootstrapWidth < 4)
		return $.el.h5(item.title);
	if(bootstrapWidth >= 4)
		return $.el.h4(item.title);
}

function changeThumbnails(pageNumber, activePage, numberOfPages, clusterId) {
	var items = clusters[clusterId].items;
	var start = (pageNumber - 1) * displayOptions.numberDisplayedItems;
	var stop = start + displayOptions.numberDisplayedItems;
	var remove = 0;
	var headerType;

	// Check if there are more spaces then items, if so, make those spaces invisible
	if(stop>items.length){
		remove = stop - items.length;
		stop = items.length;
		// console.log("Should make " + remove + " invisible.");
	}

	// console.log("start: " + start + " stop: " + stop + " page number: " + pageNumber + " current page: " + activePage + " cluster id: " + clusterId + " displayed: " + displayOptions.numberDisplayedItems + " remove: " + remove);
	var thumbIndex = 0;
	for (var i=start; i<stop; i++) {
		// console.log("Replacing thumb", thumbIndex);
		// Replace image
		$("#cluster" + clusterId + " img").eq(thumbIndex).replaceWith(
			$.el.img({'src':items[i].thumb,
					  'class':'img-responsive',
					  'alt':''}));
		// Replace title
		if(bootstrapWidth < 4)
			headerType = "h5";
		if(bootstrapWidth >= 4)
			headerType = "h4";
		$("#cluster" + clusterId + " .caption " + headerType).eq(thumbIndex).replaceWith(
				thumbnailTitle(i, items));

		// Replace id element and add new listener
		id = getId(items[i].uri)
		$("#cluster" + clusterId + " .thumbnail").eq(thumbIndex).attr("id", id);
		addClickEvent(id, items[i].link, clusterId, i);
		thumbIndex++;
	}

	// If returning from a possible invisible situation, make everything visible again
	if(activePage == numberOfPages) {
		var removed = numberOfPages * displayOptions.numberDisplayedItems - items.length;
		// console.log("Make " + removed + " thumbnail(s) visible again.");
		var start = displayOptions.numberDisplayedItems - removed;
		for(var i=start;i<displayOptions.numberDisplayedItems;i++) {
			$("#cluster" + clusterId + " .col-md-" + bootstrapWidth).eq(i).show();
		}
	}

	// Don't display unused thumbspace
	for (var i=thumbIndex; i<thumbIndex+remove;i++) {
		// Remove slow?
		$("#cluster" + clusterId + " .col-md-" + bootstrapWidth).eq(i).hide();
	}
}

function getId(uri) {
	var dirtyId = uri.substr(uri.lastIndexOf('/') + 1);
	// Only allow characters that won't trip up jQuery
	var id = dirtyId.replace(/[^\w\-]/gi, '');
	return id;
}

function addClickEvent(id, link, clusterId, index) {
	//Add thumbnail click event
	$("#cluster" + clusterId  + " #" + id).click(function() {
		//Add info to local storage to be able to save context
		localStorage.setItem("itemIndex", index);
		localStorage.setItem("clusterId", clusterId);
		localStorage.setItem("currentCluster", JSON.stringify(enrichedClusters[clusterId]));
		if((clusterId+1) == clusters.length)
			localStorage.setItem("query", "random");
		document.location.href=link;
	});
}
