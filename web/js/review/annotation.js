/*******************************************************************************
Accurator Annotation Review

Code for page allowing the review of annotations.
*******************************************************************************/
"use strict";

function annotationInit() {
	var uri = getParameterByName("uri");
	var annotations = JSON.parse(localStorage.getItem("annotations"));
	var filteredAnnotations = filterAnnotations(annotations);

	// TODO: change to check admin priviliges
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
		addNavigationButtonEvents(filteredAnnotations, uri);
	}
}

function filterAnnotations(annotationObjects) {
	var annotationArray = [];

	for (var i=0; i<annotationObjects.length; i++) {
		annotationArray[i] = annotationObjects[i].uri;
	}

	return annotationArray;
}

function addNavigationButtonEvents(annotations, uri) {
	var index = annotations.indexOf(uri);

	if (index === 0) {
		$("#annotationBtnPrevious").attr("disabled", "disabled");
	} else {
		$("#annotationBtnPrevious").click(function() {
			document.location.href = "/review/annotation.html?uri=" + annotations[index -1];
		});
	}

	if(index === annotations.length-1) {
		$("#annotationBtnNext").attr("disabled", "disabled");
	} else {
		$("#annotationBtnNext").click(function() {
			document.location.href = "/review/annotation.html?uri=" + annotations[index + 1];
		});
	}
}
