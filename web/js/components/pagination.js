/* Pagination
*  Code for initializing bootstrap pagination and handling interactions
*/
var items, labelItems;

function pagination(numberOfPages, itemsForPagination, labelForPaginationItems, clusterId) {
	// Don't use pagination if there are not enough items
	if(numberOfPages==1) return "";

	items = itemsForPagination;

	// If the list of items to be displayed is random, then clusterId is undefined
	if (clusterId === undefined) {
		clusterId = 0;
		labelItems = labelForPaginationItems + "";
	}
	else {
		labelItems = labelForPaginationItems + clusterId;
	}

	// Init HTML pagination string, starting with a disabled left arrow and an active first page
	var html = $.el.ul({'class':'pagination pagination-sm'},
					   $.el.li({'class':'disabled'},
							   $.el.span('\u00ab')),
					   $.el.li({'class':'active'},
							   $.el.span(1)));

	// Add additional pages
	for(var i = 2; i <= numberOfPages;i++){
		html.appendChild(
			$.el.li($.el.a({'href':'javascript:goToPage(' + i + ', ' + clusterId + ')'},
						   i)));
	};

	// Add right arrow to pagination
	html.appendChild(
		$.el.li($.el.a({'href':'javascript:nextPage(1' + ', ' + clusterId + ')'},
					   '\u00bb')));

	return $.el.div({'class':'row'},
					$.el.div({'class':'col-md-12'},
							 html));
}

function newPagination(numberOfPages){
	// Don't use pagination if there are not enough items
	if(numberOfPages==1) return "";

	// Init HTML pagination string, starting with a disabled left arrow and an active first page
	var html = $.el.ul({'class':'pagination pagination-sm'},
					   $.el.li({'class':'disabled'},
							   $.el.span('\u00ab')),
					   $.el.li({'class':'active'},
							   $.el.span(1)));


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

function nextPage(pageNumber, clusterId) {
	var toGoToPage = pageNumber + 1;
	//	console.log("Should be going to " + toGoToPage + " page now...");
	goToPage(toGoToPage, clusterId);
}

function previousPage(pageNumber, clusterId) {
	var toGoToPage = pageNumber - 1;
	//	console.log("Should be going to " + toGoToPage + " page now...");
	goToPage(toGoToPage, clusterId);
}

function goToPage(pageNumber, clusterId) {
	// Get the number of list items of the pagination (translation: the number of pages)
	// var numberOfPages = $("#" + labelItems + clusterId + " .pagination li").length - 2;
	// var activePage = $("#" + labelItems + clusterId + " .pagination .active").text();
	var numberOfPages = $("#" + labelItems + " .pagination li").length - 2;
	var activePage = $("#" + labelItems + " .pagination .active").text();
	// console.log("Number of pages: " + numberOfPages + " Should be going to the " + pageNumber + " page now,  current activePAge: " + activePage);
	changePagination(pageNumber, activePage, numberOfPages, clusterId);
	changeThumbnails(pageNumber, activePage, numberOfPages, items, labelItems, clusterId);
}