/******************************************************************************
Pagination
*******************************************************************************/
function Pagination(id, items, numberDisplayedItems, parentId) {
	this.numberOfPages = null; // number of pages
	this.numberOfItems = items.length; // number of items
	this.numberDisplayedItems = numberDisplayedItems; // the number of items shown
	this.page = null; // the current page
	this.node = null; // html representation of pagination row
	this.parentId = parentId; // id of the parent element (probably cluster)

	this.init();
}

Pagination.prototype.init = function() {
	this.numberOfPages = this.getNumberOfPages();
	this.page = 1;
	this.node = this.html();
}

Pagination.prototype.getNumberOfPages = function() {
	var numberOfPages = 0;
	var restPages = this.numberOfItems%this.numberDisplayedItems;

	if(restPages == 0) {
		numberOfPages = this.numberOfItems/this.numberDisplayedItems;
	} else {
		numberOfPages = (this.numberOfItems-restPages)/this.numberDisplayedItems+1;
	}
	return numberOfPages;
}

Pagination.prototype.html = function() {
	// don't use pagination if there are not enough items
	if (this.numberOfPages == 1) return "";

	// init HTML pagination, starting with a disabled left arrow and an active first page
	var html =
	$.el.ul({'class':'pagination pagination-sm'},
		$.el.li({'class':'disabled left-arrow'},
			$.el.span('\u00ab')),
		$.el.li({'class':'active'},
			$.el.span(1))
	);

	// add additional pages
	for (var i=2; i<=this.numberOfPages; i++) {
		// create html for page number
		var pageNode = $.el.li($.el.span(i));
		this.addClickEvent(pageNode, i);
		html.appendChild(pageNode);
	};

	// add right arrow to pagination
	var rightArrow =
	$.el.li({'class':'right-arrow'},
		$.el.span('\u00bb')
	);
	this.addClickNext(rightArrow);
	html.appendChild(rightArrow);

	return html;
}

Pagination.prototype.addClickEvent = function(pageNode, i) {
	var _page = this;

	$(pageNode).on("click pagination", function() {
		_page.goToPage(i);
	});
}

Pagination.prototype.addClickNext = function(pageNode) {
	var _page = this;

	$(pageNode).on("click pagination", function() {_page.next();});
}

Pagination.prototype.addClickPrevious = function(pageNode) {
	var _page = this;

	$(pageNode).on("click pagination", function() {_page.previous();});
}

Pagination.prototype.next = function() {
	this.goToPage(this.page + 1);
}

Pagination.prototype.previous = function() {
	this.goToPage(this.page - 1);
}

Pagination.prototype.goToPage = function(page) {
	var currentNode = $("#" + this.parentId + " .pagination .active");
	var newNode = $("#" + this.parentId + " .pagination li").eq(page);
	var leftArrow = $("#" + this.parentId + " .pagination .left-arrow");
	var rightArrow = $("#" + this.parentId + " .pagination .right-arrow");

	// trigger page turning event linked to this parent
	var event = jQuery.Event("pagination");
	event.currentPage = this.page;
	event.nextPage = page;
	$("#" + this.parentId).trigger(event);

	// set active class to inactive
	currentNode.removeClass("active");
	// add link to currently active page
	this.addClickEvent(currentNode, this.page);

	// set new node to active
	newNode.addClass("active");
	// remove events
	newNode.off("click pagination");
	this.page = page;

	// remove event to not add duplicates
	leftArrow.off("click pagination");
	if(page === 1) {
		// disable left arrow because switching to page one
		leftArrow.addClass("disabled");
	} else {
		// enable left arrow
		leftArrow.removeClass("disabled");
		this.addClickPrevious(leftArrow);
	}

	// remove event to not add duplicates
	rightArrow.off("click pagination");
	if(page === this.numberOfPages) {
		// disable right arrow because switching to last page
		rightArrow.addClass("disabled");
	} else {
		// enable right arrow
		rightArrow.removeClass("disabled");
		this.addClickNext(rightArrow);
	}
}
