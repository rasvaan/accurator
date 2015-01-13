/* Thumbnail
*  Code for initializing bootstrap thumbnails and handling changes
*/
function thumbnails() {
	var items = recentItems;
	var stop = displayOptions.numberDisplayedItems;
	//Check if less results available then there are to be displayed
	if(items.length<stop){
		stop = items.length;
	}
	var thumbnails = $.el.div({'class':'row'});
	for (var i=0;i<stop;i++) { 
//		console.log("Cluster items title: " + items[i].title);
		thumbnails.appendChild(
			$.el.div({'class':'col-md-2'},
					 $.el.a({'href':items[i].link},
							$.el.div({'class':'thumbnail'},
									 $.el.img({'src':items[i].thumb,
											   'class':'img-responsive',
											   'alt':''}),
									 $.el.div({'class':'caption'},
											  $.el.h6(items[i].title))))));
	}
	return thumbnails;
}

function changeThumbnails(pageNumber, activePage, numberOfPages, clusterId) {
	var items = recentItems;
	var start = (pageNumber - 1) * displayOptions.numberDisplayedItems;
	var stop = start + displayOptions.numberDisplayedItems;
	var remove = 0;
	
	// Check if there are more spaces then items, if so, make those spaces invisible
	if(stop>items.length){
		remove = stop - items.length;
		stop = items.length;
		console.log("Should make " + remove + " invisible.");
	}
	
	console.log("start: " + start + " stop: " + stop + " page number: " + pageNumber + " current page: " + activePage + " cluster id: " + clusterId + " displayed: " + displayOptions.numberDisplayedItems + " remove: " + remove);
	var thumbIndex = 0;
	for (var i=start;i<stop;i++) { 
		console.log("title: " + items[i].title + " i : " + i + " thumbIndex: " + thumbIndex);
		// Replace link
		$("#cluster" + clusterId + " .col-md-3 a").eq(thumbIndex).attr(
				'href', items[i].link);
		// Replace image
		$("#cluster" + clusterId + " img").eq(thumbIndex).replaceWith(
			$.el.img({'src':items[i].thumb,
					  'class':'img-responsive',
					  'alt':''}));
				//'<img src="'+ items[i].thumb + '" class="img-responsive" alt="">');
		// Replace title
		$("#cluster" + clusterId + " .caption h4").eq(thumbIndex).replaceWith(
			$.el.h4(items[i].title));
																			  //'<h4>' + items[i].title + '</h4>');
		thumbIndex++;
	}
	
	// If returning from a possible invisible situation, make everything visible again
	if(activePage == numberOfPages) {
		var removed = numberOfPages * displayOptions.numberDisplayedItems - items.length;
		console.log("Make " + removed + " thumbnail(s) visible again.");
		var start = displayOptions.numberDisplayedItems - removed;
		for(i=start;i<displayOptions.numberDisplayedItems;i++) {
			$("#cluster" + clusterId + " .col-md-3").eq(i).show();
		}
	}
	
	// Don't display unused thumbspace
	for (var i=thumbIndex; i<thumbIndex+remove;i++) {
		// Remove slow?
		$("#cluster" + clusterId + " .col-md-3").eq(i).hide();
	} 
}