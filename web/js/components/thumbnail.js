/******************************************************************************
Thumbnail

Code for initializing bootstrap thumbnails
*******************************************************************************/
function Thumbnail(uri, title, thumb, link, numberDisplayedItems) {
	this.id = null; // id of thumbnail
	this.uri = uri; // uri of the artwork
	this.title = title; // title of thumbnail
	this.thumb = thumb; // image to be shown
	this.link = link; // URL of location the thumbnail points to
	this.bootstrapWidth = parseInt(12/numberDisplayedItems, 10); // width of thumbnail
	this.node = null;  // html of thumbnail

	this.init();
}

Thumbnail.prototype.init = function() {
	this.id = this.getId(this.uri);
	this.node = this.html(this.id, this.thumb, this.bootstrapWidth);
}

// Retrieves the item id from the uri string
// Allow characters in the uri that won't trip up jQuery
Thumbnail.prototype.getId = function(uri) {
	var dirtyId = uri.substr(uri.lastIndexOf('/') + 1);
	var id = dirtyId.replace(/[^\w\-]/gi, '');
	return id;
}

// Generate HTML for adding a thumbnail for an item
Thumbnail.prototype.html = function(id, thumb, bootstrapWidth) {
	return $.el.div({'class':'col-md-' + bootstrapWidth},
				$.el.div({'class':'thumbnail',
						  'id':id},
					$.el.img({'src':thumb,
							  'class':'img-responsive',
							  'alt':''}),
						$.el.div({'class':'caption'},
							 this.thumbnailTitle())));
}

Thumbnail.prototype.setImage = function(url) {
	// replace image url
	this.thumb = url;
	$(this.node).find("img").attr("src", url);
}

Thumbnail.prototype.setTitle = function(title) {
	this.title = title;
	$(this.node).find(".thumbnailHdrTitle").html(title);
}

Thumbnail.prototype.setId = function(id) {
	this.id = id;
	$(this.node).find(".thumbnail").attr("id", id);
}

Thumbnail.prototype.show = function() {
	$(this.node).show();
}

Thumbnail.prototype.hide = function() {
	$(this.node).hide();
}

// Retrieves the title of an item and sizes it accordingly
Thumbnail.prototype.thumbnailTitle = function() {
	//Make header depent on size thumbnail
	if(this.bootstrapWidth < 4)
		return $.el.h5({'class':'thumbnailHdrTitle'}, this.title);
	if(this.bootstrapWidth >= 4)
		return $.el.h4({'class':'thumbnailHdrTitle'}, this.title);
}

// Add thumbnail click event
Thumbnail.prototype.setClickEvent = function(link, cluster) {
	var _thumbnail = this;
	this.link = link; // update link

	// remove possible old event
	$(this.node).find(".thumbnail").off("click thumbnail");

	$(this.node).find(".thumbnail").on("click thumbnail", function() {
		// add info to local storage to be able to save context
		localStorage.setItem("cluster", JSON.stringify(cluster));
		document.location.href = _thumbnail.link;
	});
}
