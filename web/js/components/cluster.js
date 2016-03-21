/*******************************************************************************
Cluster
*******************************************************************************/
function Cluster(uris, id) {
	this.id = id; // id of the cluster
	this.uris = uris; // list of uris of the items
	this.items = []; // enriched items
}

Cluster.prototype.enrich = function() {
	var _cluster = this; //make sure we can use this Cluster in $ scope

	return $.ajax({
		type: "POST",
		url: "metadata",
		contentType: "application/json",
		data: JSON.stringify({"uris":this.uris})})
	.then(function(data) {
		// Enrich one image element in the cluster adding an image,
		// a link where it can be (further) annotated and a title
		var items = [];

		for(var i=0; i<data.length; i++) {
			items[i] = {};
			var uri = data[i].uri;
			items[i].uri = uri;
			items[i].thumb = data[i].thumb;
			items[i].link = "item.html?uri=" + uri;
			items[i].title = truncate(data[i].title, 60);
		}
		_cluster.items = items;
	 });
}

Cluster.prototype.display = function(numberDisplayedItems) {
	var numberOfPages = this.getNumberOfPages(this.items.length, numberDisplayedItems);

	$("#" + this.id).append(
		pagination(numberOfPages, this.items, "cluster", this.id)
	);
	this.addThumbnails(numberDisplayedItems);
}

Cluster.prototype.getNumberOfPages = function(numberOfItems, numberDisplayedItems) {
	var numberOfPages = 0;
	var restPages = numberOfItems%numberDisplayedItems;

	if(restPages == 0) {
		numberOfPages = numberOfItems/numberDisplayedItems;
	} else {
		numberOfPages = (numberOfItems-restPages)/numberDisplayedItems+1;
	}
	return numberOfPages;
}

// Add thumbnails for a cluster
Cluster.prototype.addThumbnails = function(numberDisplayedItems) {
	var stop = numberDisplayedItems;
	var bootstrapWidth = parseInt(12/numberDisplayedItems, 10);

	console.log("number items", this.items.length);
	// check if less results available then there are to be displayed
	if(this.items.length < stop){
		stop = this.items.length;
	}

	// add row
	$("#" + this.id).append(
		$.el.div({'class':'row', 'id':'thumbnailRow' + this.id}));

	for (var i=0; i<stop; i++) {
		var thumbnail = new Thumbnail(
			items[i].uri,
			items[i].title,
			items[i].thumb,
			numberDisplayedItems
		);

		$("#thumbnailRow" + this.id).append(thumbnail.node);
		thumbnail.addClickEvent(id, items[i].link, clusterId, i);
	}
}

Cluster.prototype.changeThumbnails = function(pageNumber, activePage, numberOfPages, items, labelItems, clusterId) {
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
