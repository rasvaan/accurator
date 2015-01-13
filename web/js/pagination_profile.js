/* Pagination
*  Code for initializing bootstrap pagnination and handling interactions
*/
function pagination(numberOfPages, clusterId) {
	// Don't use pagination if there are not enough items
	if(numberOfPages==1) return "";
	
	// Init HTML pagination string, starting with a disabled left arrow and an active first page
	var html = $.el.ul({'class':'pagination pagination-sm'},
					   $.el.li({'class':'disabled'},
							   $.el.span('\u00ab')),
					   $.el.li({'class':'active'},
							   $.el.span(1)));
	
	// Add additional pages
	for(var i=2;i<=numberOfPages;i++){
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

function changePagination(pageNumber, activePage, numberOfPages, clusterId) {
	//Often replacing html, this is because often <a> has to be replaced by <span> and vice versa 
	//Replace span active class with a link
	$("#cluster" + clusterId + " .pagination li").eq(activePage).replaceWith(
		$.el.li($.el.a({'href':'javascript:goToPage(' + activePage + ', ' + clusterId + ')'},
					   activePage)));
																			 
	//Replace a link with span active class
	$("#cluster" + clusterId + " .pagination li").eq(pageNumber).replaceWith(
		$.el.li({'class':'active'},
				$.el.span(pageNumber)));

	//Replace left arrow
	if(pageNumber == 1) {
		// Disable left because switching to page one
		$("#cluster" + clusterId + " .pagination li").eq(0).replaceWith(
			$.el.li({'class':'disabled'},
					$.el.span('\u00ab')));
	} else {
		// Enable left with pageNumber as a link
		$("#cluster" + clusterId + " .pagination li").eq(0).replaceWith(
			$.el.li($.el.a({'href':'javascript:previousPage(' + pageNumber + ', ' + clusterId + ')'},
						   '\u00ab')));
	}
	
	//Replace right arrow
	if(pageNumber == numberOfPages) {
		// Disable left because switching to page one
		$("#cluster" + clusterId + " .pagination li").eq(numberOfPages+1).replaceWith(
			$.el.li({'class':'disabled'},
					$.el.span('\u00bb')));
	} else {
		// Enable left with pageNumber as a link
		$("#cluster" + clusterId + " .pagination li").eq(numberOfPages+1).replaceWith(
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
	var numberOfPages = $("#cluster" + clusterId + " .pagination li").length - 2;
	var activePage = $("#cluster" + clusterId + " .pagination .active").text();
//	console.log("Number of pages: " + numberOfPages + " Should be going to the " + pageNumber + " page now,  current activePAge: " + activePage);
	changePagination(pageNumber, activePage, numberOfPages, clusterId);
	changeThumbnails(pageNumber, activePage, numberOfPages, clusterId);
}