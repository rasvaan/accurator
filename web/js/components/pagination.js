/******************************************************************************
Pagination
*******************************************************************************/
function Pagination(id, items, numberDisplayedItems, parentId) {
	this.numberOfPages = null; // number of pages
	this.numberOfItems = items.length; // number of items
	this.numberDisplayedItems = numberDisplayedItems; // the number of items shown
	this.page = null; // the current page
	this.items = items; // TODO: remove? items presented by pagination
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
		var pageNode = $.el.li($.el.a({'href':'#'}, i));
		this.addClickEvent(pageNode, i);
		html.appendChild(pageNode);
	};

	// add right arrow to pagination
	var rightArrow =
	$.el.li(
		$.el.a({'href':'#', 'class':'right-arrow'},
		 	'\u00bb')
	);
	this.addClickNext(rightArrow);
	html.appendChild(rightArrow);

	return $.el.div({'class':'row'},
				$.el.div({'class':'col-md-12'},
					html));
}

Pagination.prototype.addClickEvent = function(pageNode, i) {
	var _page = this;

	$(pageNode).on("click", function() {
		_page.goToPage(i);
	});
}

Pagination.prototype.addClickNext = function(pageNode) {
	var _page = this;

	$(pageNode).on("click", function() {_page.next();});
}

Pagination.prototype.addClickPrevious = function(pageNode) {
	var _page = this;

	$(pageNode).on("click", function() {_page.previous();});
}

Pagination.prototype.next = function() {
	this.goToPage(this.page + 1);
}

Pagination.prototype.previous = function() {
	this.goToPage(this.page - 1);
}

Pagination.prototype.goToPage = function(page) {
	console.log("Current page ", this.page, " and dhould be going to " + page + " page now...");

	// add link to currently active page

	// set active class to inactive
	// $("#" + this.parentId + " .pagination .active").removeClass("active");

	// 	$.el.li(
	// 		$.el.a({'href':'javascript:goToPage(' + this.page + ', ' + this.parentId + ')'},
	// 			this.page))
	// );
	//
	// // replace a link with span active class
	// $("#" + this.parentId + " .pagination li").eq(pageNumber).replaceWith(
	// 	$.el.li({'class':'active'},
	// 		$.el.span(pageNumber))
	// );
	//
	// // replace left arrow
	// if(pageNumber == 1) {
	// 	// disable left because switching to page one
	// 	$("#" + this.parentId + " .pagination li").eq(0).replaceWith(
	// 		$.el.li({'class':'disabled'},
	// 			$.el.span('\u00ab'))
	// 	);
	// } else {
	// 	// enable left with pageNumber as a link
	// 	$("#" + this.parentId + " .pagination li").eq(0).replaceWith(
	// 		$.el.li(
	// 			$.el.a({'href':'javascript:previousPage(' + pageNumber + ', ' + this.parentId + ')'},
	// 				'\u00ab'))
	// 	);
	// }
	//
	// // replace right arrow
	// if(pageNumber == numberOfPages) {
	// 	// disable left because switching to page one
	// 	$("#" + labelItems + " .pagination li").eq(numberOfPages+1).replaceWith(
	// 		$.el.li({'class':'disabled'},
	// 				$.el.span('\u00bb')));
	// } else {
	// 	// enable left with pageNumber as a link
	// 	$("#" + labelItems + " .pagination li").eq(numberOfPages+1).replaceWith(
	// 		$.el.li(
	// 			$.el.a({'href':'javascript:nextPage(' + pageNumber + ', ' + clusterId + ')'},
	// 				'\u00bb'))
	// 	);
	// }
	// changePagination(pageNumber, activePage, numberOfPages, clusterId);
	// changeThumbnails(pageNumber, activePage, numberOfPages, items, labelItems, clusterId);
}

function changePagination(pageNumber, activePage, numberOfPages, clusterId) {
	console.log("labeItems=", labelItems);
	//Often replacing html, this is because often <a> has to be replaced by <span> and vice versa
	//Replace span active class with a link
	// $("#" + labelItems + clusterId + " .pagination li").eq(activePage).replaceWith(
	$("#" + labelItems + " .pagination li").eq(activePage).replaceWith(
		$.el.li($.el.a({'href':'javascript:goToPage(' + activePage + ', ' + clusterId + ')'},
					   activePage)));

	//Replace a link with span active class
	// $("#" + labelItems + clusterId + " .pagination li").eq(pageNumber).replaceWith(
	$("#" + labelItems + " .pagination li").eq(pageNumber).replaceWith(
		$.el.li({'class':'active'},
				$.el.span(pageNumber)));

	//Replace left arrow
	if(pageNumber == 1) {
		// Disable left because switching to page one
		// $("#" + labelItems + clusterId + " .pagination li").eq(0).replaceWith(
		$("#" + labelItems + " .pagination li").eq(0).replaceWith(
			$.el.li({'class':'disabled'},
					$.el.span('\u00ab')));
	} else {
		// Enable left with pageNumber as a link
		// $("#" + labelItems + clusterId + " .pagination li").eq(0).replaceWith(
		$("#" + labelItems + " .pagination li").eq(0).replaceWith(
			$.el.li($.el.a({'href':'javascript:previousPage(' + pageNumber + ', ' + clusterId + ')'},
						   '\u00ab')));
	}

	//Replace right arrow
	if(pageNumber == numberOfPages) {
		// Disable left because switching to page one
		// $("#" + labelItems + clusterId + " .pagination li").eq(numberOfPages+1).replaceWith(
		$("#" + labelItems + " .pagination li").eq(numberOfPages+1).replaceWith(
			$.el.li({'class':'disabled'},
					$.el.span('\u00bb')));
	} else {
		// Enable left with pageNumber as a link
		// $("#" + labelItems + clusterId + " .pagination li").eq(numberOfPages+1).replaceWith(
		 $("#" + labelItems + " .pagination li").eq(numberOfPages+1).replaceWith(
			$.el.li($.el.a({'href':'javascript:nextPage(' + pageNumber + ', ' + clusterId + ')'},
						   '\u00bb')));
	}
}
