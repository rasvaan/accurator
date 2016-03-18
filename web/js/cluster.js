/*******************************************************************************
Cluster
*******************************************************************************/
function Cluster(uris, id) {
	this.uris = uris;
	this.id = id;
	this.items = [];
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

Cluster.prototype.display = function() {
	var numberOfPages = this.getNumberOfPages(this.items.length);

	$("#" + this.id).append(
		pagination(numberOfPages, this.items, "cluster", this.id)
	);
	thumbnails(this.id);
}

Cluster.prototype.getNumberOfPages = function(numberOfItems) {
	var numberOfPages = 0;
	var restPages = numberOfItems%display.numberDisplayedItems;

	if(restPages == 0) {
		numberOfPages = numberOfItems/display.numberDisplayedItems;
	} else {
		numberOfPages = (numberOfItems-restPages)/display.numberDisplayedItems+1;
	}
	return numberOfPages;
}
