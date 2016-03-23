/*******************************************************************************
Cluster
*******************************************************************************/
function Cluster(uris, id) {
	this.id = id; // id of the cluster
	this.uris = uris; // list of uris of the items
	this.items = []; // enriched items
	this.thumbnails = []; // thumbnails
	this.pagination = null;
	this.node = null;
	this.enriched = false;

	this.init();
}

Cluster.prototype.init = function() {
	this.node = this.html();
}

Cluster.prototype.html = function() {
	return $.el.div({'class':'well well-sm', 'id':this.id});
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
		_cluster.enriched = true;
	 });
}

Cluster.prototype.display = function(numberDisplayedItems) {
	// draw the pagination and thumbnails
	//this.addPath();
	this.addPagination(numberDisplayedItems);
	this.addThumbnails(numberDisplayedItems);
}

Cluster.prototype.addPagination = function(numberDisplayedItems) {
	var _cluster = this;
	var paginationId = this.id + "Pagination"

	this.pagination = new Pagination(
		paginationId,
		this.items,
		numberDisplayedItems,
		this.id
	);

	$(this.node).append(
		// add pagination row
		$.el.div({'class':'row'},
			$.el.div({'class':'col-md-12'},
				this.pagination.node))
	);

	// add event listener for change of page
	$("#" + this.id).on("pagination", function(event) {
		_cluster.changeThumbnails(event.currentPage, event.nextPage, numberDisplayedItems);
	});
}


// Add thumbnails for a cluster
Cluster.prototype.addThumbnails = function(numberDisplayedItems) {
	var stop = numberDisplayedItems;
	var bootstrapWidth = parseInt(12/numberDisplayedItems, 10);

	// check if less results available then there are to be displayed
	if(this.items.length < stop){
		stop = this.items.length;
	}

	// add row
	$(this.node).append(
		$.el.div({'class':'row', 'id':'thumbnailRow' + this.id}));

	for (var i=0; i<stop; i++) {
		var thumbnail = new Thumbnail(
			this.items[i].uri,
			this.items[i].title,
			this.items[i].thumb,
			this.items[i].link,
			numberDisplayedItems
		);
		$(this.node).find("#thumbnailRow" + this.id).append(
			thumbnail.node
		);
		thumbnail.setClickEvent(this.items[i].link, this.id);
		this.thumbnails[i] = thumbnail;
	}
}

Cluster.prototype.changeThumbnails = function(currentPage, nextPage, numberDisplayedItems) {
	var bootstrapWidth = parseInt(12/numberDisplayedItems, 10); // width of thumbnail
	var numberOfPages = this.pagination.numberOfPages;
	var start = (nextPage - 1) * numberDisplayedItems; // start index of items shown
	var stop = start + numberDisplayedItems; // stop index of items shown
	var remove = 0; // number of thumbnails spaces not shown
	var headerType; // size of the header

	// Check if there are more spaces then items, if so, make those spaces invisible
	if(stop > this.items.length) {
		remove = stop - this.items.length;
		stop = this.items.length;
	}

	console.log("start: " + start + " stop: " + stop + " page number: " + nextPage + " current page: " + currentPage + " cluster id: " + this.id + " displayed: " + numberDisplayedItems + " remove: " + remove);
	var thumbIndex = 0; // index of the thumbnail spots
	for (var i=start; i<stop; i++) {
		var thumbnail = this.thumbnails[thumbIndex];

		thumbnail.setImage(this.items[i].thumb);
		thumbnail.setTitle(this.items[i].title);
		thumbnail.setId(thumbnail.getId(this.items[i].uri));
		thumbnail.setClickEvent(this.items[i].link, this.id);

		thumbIndex++;
	}

	// if returning from a possible invisible situation, make everything visible again
	if(currentPage === numberOfPages) {
		var removed = numberOfPages * numberDisplayedItems - this.items.length;
		var start = numberDisplayedItems - removed;

		for(var i = start; i < numberDisplayedItems; i++) {
			this.thumbnails[i].show();
		}
	}

	// don't display unused thumbspace
	for (var i = thumbIndex; i < thumbIndex+remove; i++) {
		this.thumbnails[i].hide();
	}
}
