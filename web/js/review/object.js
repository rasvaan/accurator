/*******************************************************************************
Accurator Object Review

Code for page allowing the review of annotations.
*******************************************************************************/
"use strict";

function objectInit() {
	var uri = getParameterByName("uri");
	var objects = JSON.parse(localStorage.getItem("annotated_objects"));
	var filteredObjects = filterObjects(objects);

	adminLoggedIn()
	.then(function() {
		// user is logged in as admin, so draw page
		drawPage();
	}, function() {
		// user is not logged in as admin, show modal
		var onDismissal = function() {document.location.href = "/intro.html";};
		login(drawPage, onDismissal, "admin");
	});

	function drawPage() {
		metadata(uri);
		addNavigationButtonEvents(filteredObjects, uri);
	}
}

function metadata(uri) {
	return $.getJSON("/metadata", {uri:uri})
	.then(function(metadata){
		// set src img
		$(".objectImg").attr("src", metadata.image);

		return metadata;
	});
}

function filterObjects(annotationObjects) {
	var objectArray = [];

	for (var i=0; i<annotationObjects.length; i++) {
		objectArray[i] = annotationObjects[i].uri;
	}

	return objectArray;
}

function addNavigationButtonEvents(objects, uri) {
	var index = objects.indexOf(uri);

	if (index === 0) {
		$("#objectBtnPrevious").attr("disabled", "disabled");
	} else {
		$("#objectBtnPrevious").on("click", function() {
			document.location.href = "/review/object.html?uri=" + objects[index -1];
		});
	}

	if(index === objects.length-1) {
		$("#objectBtnNext").attr("disabled", "disabled");
	} else {
		$("#objectBtnNext").on("click", function() {
			document.location.href = "/review/object.html?uri=" + objects[index + 1];
		});
	}
}
