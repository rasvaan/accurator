/*******************************************************************************
Accurator Object Review

Code for page allowing the review of annotations.
*******************************************************************************/
"use strict";

function objectInit() {
	var uri = getParameterByName("uri");
	var objects = JSON.parse(localStorage.getItem("annotated_objects"));
	var object = getObject(objects, uri);
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

		// add annotations to the interface
		for (var i=0; i<object.annotations.length; i++) {
			addRow(object.annotations[i]);
		}
	}
}

function getObject(objects, uri) {
	for (var i=0; i<objects.length; i++) {
		if (objects[i].uri === uri)
			return objects[i];
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

function addRow(annotation) {
	var rowId = 'objectTr' + generateIdFromUri(annotation.uri);
	var buttons = new ReviewButtons(annotation.uri, rowId, "sm");

	$(".objectTblAnnotations").append(
		$.el.tr(
			{'id':rowId},
			$.el.td(
				annotation.label
			),
			$.el.td(
				annotation.type
			),
			$.el.td(
				getUserName(annotation.annotator)
			),
			$.el.td(
				buttons.node
			)
	));

	colorRow(rowId, annotation);
}

function normalizeTitle(object) {
	var normalized = object.title;

	if (object.title === "no_title") {
		// create title from id
		normalized = generateIdFromUri(object.uri);
	}

	return truncate(normalized, 60);
}

function colorRow(rowId, annotation) {
	// color the row according to the review made
	if (annotation.reviews.length > 0) {
		// sort reviews to base color on latest
		annotation.reviews.sort(function(a, b){
		    var keyA = new Date(a.time);
		    var keyB = new Date(b.time);

		    // compare the 2 dates
		    if(keyA < keyB) return -1;
		    if(keyA > keyB) return 1;
		    return 0;
		});

		if (annotation.reviews[0].judgement === "agree") {
			$("#" + rowId).addClass('success');
		} else if (annotation.reviews[0].judgement === "unsure") {
			$("#" + rowId).addClass('warning');
		} else if (annotation.reviews[0].judgement === "disagree") {
			$("#" + rowId).addClass('danger');
		}
	}
}
