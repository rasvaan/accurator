// var initialClusters = [];
// var enrichedClusters = [];
// var clusters = [];
// var query = "";

// displayOptions = {
// 	numberDisplayedItems: 4,
// 	showFilters: true,
// 	imageFilter: 'onlyImages',
// }

// function search(keyword, target) {
// 	query  = keyword;
// 	$(document).prop('title', 'Searching for ' + query);
// 	$("#results").append(searchingHtml());
//
// 	onDone = function(data){
// 		$("#results").children().remove();
// 		showFilters();
// 		processJsonResults(data);
// 		createResultClusters();
// 		$(document).prop('title', 'Results for ' + query);
// 	};
//
// 	onFail = function(data, textStatus){
// 		$("#results").children().remove();
// 		$("#results").append(errorHtml(data, textStatus));
// 		$(document).prop('title', 'Error on ' + query);
// 	};
//
// 	//Get and process clusters
// 	if (typeof target == 'undefined') {
// 		$.getJSON("cluster_search_api", {query:query})
// 		.done(onDone)
// 		.fail(onFail);
// 	} else {
// 		$.getJSON("cluster_search_api", {query:query,
// 										 target:target})
// 		.done(onDone)
// 		.fail(onFail);
// 	}
// }

function searchingHtml(){
	return $.el.div({'class':'row'},
					$.el.div({'class':'col-lg-10 col-md-offset-1'},
							 $.el.h3('Searching for ',
									 $.el.span({'class':'text-info'},
											   query))));
}

function errorHtml(data, textStatus){
	return $.el.div({'class':'row'},
					$.el.div({'class':'col-lg-10 col-md-offset-1'},
							 $.el.h3('Unfortunately an ',
									 $.el.span({'class':'text-danger'},
											   'error'),
									 ' has occured:')),
					$.el.div({'class':'row'},
							 $.el.div({'class':'well well-sm col-md-10 col-md-offset-1'},
									  data.responseText)));
}

function noResultsHtml(query) {
	return $.el.div({'class':'row'},
					$.el.div({'class':'col-lg-10 col-md-offset-1'},
							 $.el.h3('No results found for ',
									 $.el.span({'class':'text-danger'},
											   query))));
}

function noFilterResultsHtml() {
	return $.el.h4('No results due to filter: ',
				   $.el.span({'class':'text-danger'},
							 displayOptions.imageFilter));
}

function showFilters() {
	// console.log('Showing filters:', displayOptions.showFilters);
	if(displayOptions.showFilters) {
		$("#results").append(filterAndRankButtons());
	}
}

function processJsonResults(data) {
	// Convert json to initialClusters array
	var sourceClusters = data.clusters;
	var numberOfClusters = sourceClusters.length;

	for (var i=0;i<numberOfClusters;i++) {
		// Get path uris and query for the labels
		var path = sourceClusters[i].path;
		var numberOfItems = sourceClusters[i].results;
		var items = [];

		for (var j=0;j<numberOfItems;j++) {
			var uri = sourceClusters[i].items[j].uri;
			items[j] = new item(uri);
		}
		initialClusters[i] = new cluster(path, items);
	}
}

function createResultClusters() {
	if(initialClusters.length == 0){
		// console.log('No results found for ', query);
		$("#results").append(noResultsHtml(query));
	} else {
		for(var i=0;i<initialClusters.length;i++) {
			$("#results").append(clusterContainer(i));
			// Append path to cluster container
			addPath(i);
			// Add enriched clusters and pagination
			addItems(i);
		}
	}
}

function clusterContainer(clusterId) {
	return $.el.div({'class':'well well-sm',
					'id':'cluster' + clusterId});
}

function addPath(clusterId) {
	// Generate HTML cluster path
	var pathUris = initialClusters[clusterId].path;
	var pathElements = $.el.h4();
	enrichedClusters[clusterId] = new cluster('undefined', 'undefined');

	// Get labels from server
	var json = {"uris":pathUris, "type":"label"};
	$.ajax({type: "POST",
			url: "metadata",
			contentType: "application/json",
			data: JSON.stringify(json),
			success: function(labels) {
				var path = [];

				for(var i=0; i<pathUris.length; i++)
					path[i] = {uri:pathUris[i], label:labels[i]};

				// Add path to enrichedClusters for future reference
				path.reverse();
				enrichedClusters[clusterId].path = path;
				$("#cluster"+clusterId).prepend(pathHtmlElements(path));
				unfoldPathEvent("#cluster"+clusterId, path);
		   }
	});
}

function pathHtmlElements(path) {
	var simplePathElements = $.el.h4();

	//Simplified
	if(path.length==0){
		// Only show query in case there is no path
		simplePathElements.appendChild(
			$.el.span({'class':'path-label path-literal'},
					  query));
	} else {
		simplePathElements.appendChild(
			$.el.span({'class':'path-label path-property'},
					  path[path.length-2].label));

		simplePathElements.appendChild(
			$.el.span({'class':'path-label path-resource'},
					  path[path.length-3].label));
	}
	return $.el.div({'class':'row path'},
					$.el.div({'class':'col-md-12'},
							 simplePathElements));
}

function unfoldPathEvent(id, path) {
	var pathElements = $.el.h4();

	if(path.length==0){
		// Only show query in case there is no path
		pathElements.appendChild(
			$.el.span({'class':'path-label path-literal'},
					  query));
	} else {
		for(var i=0; i<path.length; i++) {
			// Label colouring
			if(i==0){
				pathElements.appendChild(
					$.el.span({'class':'path-label path-literal'},
							query));
			} else if(i%2==0){
				pathElements.appendChild(
					$.el.span({'class':'path-label path-resource'},
						path[i].label));
			} else {
				pathElements.appendChild(
					$.el.span({'class':'path-label path-property'},
						path[i].label));
			}
			// Add arrow if not end of path
			if(!(path.length==i+1)){
				pathElements.appendChild(
					$.el.span({'class':'glyphicon glyphicon-arrow-right'}));
			}
		}
	};

	$(id + " .path-label").click(function() {
		$(id + " .path").html(
			$.el.div({'class':'col-md-12'},
					pathElements)
		);
	});
}

function addItems(clusterId) {
	var items = initialClusters[clusterId].items;
	var uris = [];
	var enrichedItems = [];

	for(var i=0; i<items.length; i++)
		uris[i] = items[i].uri;

	var json = {"uris":uris};
	$.ajax({type: "POST",
			url: "metadata",
			contentType: "application/json",
			data: JSON.stringify(json),
			success: function(data) {
				processEnrichment(data, clusterId);
				// Clone cluster to enable filtering without losing information.
				clusters[clusterId] = clone(enrichedClusters[clusterId]);
				filterCluster(clusters[clusterId]);
				if(clusters[clusterId].items.length==0) {
					$("#cluster"+clusterId).append(noFilterResultsHtml());
				} else {
					var pages = determineNumberOfPages(clusterId);
					$("#cluster"+clusterId).append(pagination(pages, clusterId));
					thumbnails(clusterId);
				}
		   }
	});
}

function processEnrichment(data, clusterId) {
	var items = [];

	for(var i=0; i<data.length; i++) {
		var uri = data[i].uri;
		var thumb = data[i].thumb;
		var link = "annotate.html?uri=" + uri;
		var title = truncate(data[i].title, 60);
		items[i] = new item(uri, thumb, link, title);
	}
	// Add items to enrichedClusters for future reference
	enrichedClusters[clusterId].items = items;
}

function determineNumberOfPages (clusterId) {
	var numberOfPages = 0;
	var numberOfItems = clusters[clusterId].items.length;
	var restPages = numberOfItems%displayOptions.numberDisplayedItems;

	//Determine number of items in pagination
	if(restPages == 0) {
		numberOfPages = numberOfItems/displayOptions.numberDisplayedItems;
	} else {
		numberOfPages = (numberOfItems-restPages)/displayOptions.numberDisplayedItems+1;
	}
	return numberOfPages;
}

function filterAndRankButtons() {
	return $.el.div({'class':'row'},
					$.el.div({'class':'col-md-12 filters'},
							 filterButtons()));
							 //rankButtons()));
}

function filterButtons() {
	return $.el.div({'class':'btn-group'},
			 	    $.el.button({'type':'button',
								 'class':'btn btn-default dropdown-toggle',
							     'data-toggle':'dropdown'},
							     'Image Filter ',
							    $.el.span({'class':'caret'})),
				    $.el.ul({'class':'dropdown-menu',
						     'role':'menu'},
						     $.el.li(
									 $.el.a({'href':'javascript:filterTrigger(\'onlyImages\')'},
										     'Only Images'),
									 $.el.a({'href':'javascript:filterTrigger(\'allObjects\')'},
										     'All Objects'))));
}

function rankButtons() {
	return $.el.div({'class':'btn-group'},
					$.el.button({'type':'button',
								'class':'btn btn-default dropdown-toggle',
								'data-toggle':'dropdown'},
								'Cluster Abstraction ',
								$.el.span({'class':'caret'})),
					$.el.ul({'class':'dropdown-menu',
							 'role':'menu'},
							 $.el.li(
									 $.el.a({'href':'javascript:clusterAbstractionTrigger(\'instanceLevel\')'},
											 'Instance Level'),
									 $.el.a({'href':'javascript:clusterAbstractionTrigger(\'classLevel\')'},
											 'Class Level'),
									 $.el.a({'href':'javascript:clusterAbstractionTrigger(\'whatLevel\')'},
											 'Who What Where'))));
}

function filterTrigger(type) {
	var changeShownContent = false;
	// console.log('Filtering type: ', type);
	if(type == 'onlyImages' && !(displayOptions.imageFilter == 'onlyImages')) {
		displayOptions.imageFilter = 'onlyImages';
		changeShownContent = true;
	}
	if(type == 'allObjects'  && !(displayOptions.imageFilter == 'allObjects')) {
		displayOptions.imageFilter = 'allObjects';
		changeShownContent = true;
	}
	// console.log('Should change something', changeShownContent);
	if(changeShownContent) {
		clusters = clone(enrichedClusters);
		filter();
		updateClusters();
	}
}

function clusterAbstractionTrigger(type) {
	var changeClusterAbstraction = false;
	if(type == 'instanceLevel' && !(displayOptions.imageFilter == 'instanceLevel')) {
		displayOptions.imageFilter = 'instanceLevel';
		changeClusterAbstraction = true;
	}
	if(type == 'classLevel'  && !(displayOptions.imageFilter == 'classLevel')) {
		displayOptions.imageFilter = 'classLevel';
		changeClusterAbstraction = true;
	}
	if(type == 'whatLevel'  && !(displayOptions.imageFilter == 'whatLevel')) {
		displayOptions.imageFilter = 'whatLevel';
		changeClusterAbstraction = true;
	}
	if(changeClusterAbstraction) {
		// console.log("Should be changing the Cluster Abstraction Level");
		updateClusters();
	}
}

function updateClusters() {
	for(var i=0; i<clusters.length; i++) {
		// console.log('Adding clusters', clusters);
		if(clusters[i].items.length==0) {
			$("#cluster"+i).children().remove();
			$("#cluster"+i).append(pathHtmlElements(clusters[i].path, i));
			$("#cluster"+i).append(noFilterResultsHtml());
		} else {
			$("#cluster"+i).children().remove();;
			$("#cluster"+i).append(pathHtmlElements(clusters[i].path, i));
			var pages = determineNumberOfPages(i);
			$("#cluster"+i).append(pagination(pages, i));
			$("#cluster"+i).append(thumbnails(i));
		}
	}
}

function filter() {
	if(displayOptions.imageFilter == 'onlyImages') {
		// console.log('Filtering out objects without image');
		// Remove all items without an image (read: with a stub)
		for(var i=0; i<clusters.length; i++) {
			filterCluster(clusters[i]);
		}
	}
}

function filterCluster(cluster) {
	if(displayOptions.imageFilter == 'onlyImages') {
		// Remove all items without an image (read: with a stub)
		var items = cluster.items;
		// Check which items should be removed
		for(var i=0; i<items.length; i++) {
			if(items[i].thumb.substring(items[i].thumb.length-8) == "stub.png") {
				items.splice(i, 1);
				// Move the index one back because of removal
				i--;
			}
		}
	}
}

function rankClusters() {
	// Sort on size for now
	// console.log("Custers: " + clusters);
	clusters.sort(compareClusterSize);
}

function compareClusterSize(a, b) {
	if (a.items.length < b.items.length) return 1;
	if (a.items.length > b.items.length) return -1;
	return 0;
}

function cluster(path, items) {
	this.path = path;
	this.items = items;
}

function item(uri, thumb, link, title) {
	this.uri = uri;
	this.thumb = thumb;
	this.link = link;
	this.title = title;
}

function clone(obj) {
    // Handle the 3 simple types, and null or undefined
    if (null == obj || "object" != typeof obj) return obj;
    // Handle Date
    if (obj instanceof Date) {
        var copy = new Date();
        copy.setTime(obj.getTime());
        return copy;
    }
    // Handle Array
    if (obj instanceof Array) {
        var copy = [];
        for (var i = 0, len = obj.length; i < len; i++) {
            copy[i] = clone(obj[i]);
        }
        return copy;
    }
    // Handle Object
    if (obj instanceof Object) {
        var copy = {};
        for (var attr in obj) {
            if (obj.hasOwnProperty(attr)) copy[attr] = clone(obj[attr]);
        }
        return copy;
    }
    throw new Error("Unable to copy obj! Its type isn't supported.");
}

function truncate (string, limit) {
	var chars;
	var i;

	chars = string.split('');
	if (chars.length > limit) {
		for (var i=chars.length - 1; i>-1; --i) {
			if (i>limit) {
				chars.length = i;
			}
			else if (' ' === chars[i]) {
				chars.length = i;
				break;
			}
		}
		chars.push('...');
	}
	return chars.join('');
}

// function setGlobalQuery(keyword) {
// 	query = keyword;
// }
