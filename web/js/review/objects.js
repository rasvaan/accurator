/*******************************************************************************
Accurator Annotations

Code for showing the annotations.
*******************************************************************************/
"use strict";

function objectsInit() {
	var domain = getParameterByName("domain");
	clearLocalStorage("annotated_objects"); // will be generating new list of annotations

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
		// add title
		$("#objectsHdrSlogan").prepend("Annotated objects in " + domain + " domain");

		getAnnotations(domain)
		.then(function(annotations) {
			var objects = objectAnnotations(annotations);

			// add annotated objects to local storage
			localStorage.setItem("annotated_objects", JSON.stringify(objects));

			// add objects to the interface
			for (var i=0; i<objects.length; i++) {
				addRow(objects[i]);
			}
		});
	}
}

function objectAnnotations(annotations) {
	var objects = [];

	for (var i=0; i<annotations.length; i++) {
		var objectIndex = -1;

		for (var j=0; j<objects.length; j++) {
			// check if already present and record index
			if (objects[j].uri === annotations[i].object.uri) {
				objectIndex = j;
			}
		}

		var objectAnnotation = JSON.parse(JSON.stringify(annotations[i])); //clone
		delete objectAnnotation.object;

		if (objectIndex >= 0) {
			// add annotation to existing object
			var annotationIndex = objects[objectIndex].annotations.length;
			objects[objectIndex].annotations[annotationIndex] = objectAnnotation;
		} else {
			// create new object
			var object = {
				'title': annotations[i].object.title,
				'uri': annotations[i].object.uri,
				'annotations': [objectAnnotation]
			}
			objects[objects.length] = object;
		}
	}

	return objects;
}

function addRow(object) {
	var rowId = 'objectsTr' + generateIdFromUri(object.uri);
	var title = normalizeTitle(object);
	var reviewed = countReviewed(object);

	$(".objectsTblObjects").append(
		$.el.tr(
			{'id':rowId},
			$.el.td(
				$.el.a(
					{'href': "/review/object.html?uri=" + object.uri},
					title
			)),
			$.el.td(
				object.annotations.length
			),
			$.el.td(
				reviewed
			)
	));

	if (reviewed === object.annotations.length)
		$("#" + rowId).addClass('success');
}

function normalizeTitle(object) {
	var normalized = object.title;

	if (object.title === "no_title") {
		// create title from id
		normalized = generateIdFromUri(object.uri);
	}

	return truncate(normalized, 60);
}

function countReviewed(object) {
	var reviewed = 0;

	for(var i=0; i<object.annotations.length; i++) {
		// count the number of annotations with one or more reviews
		if (object.annotations[i].reviews.length > 0)
			reviewed++;
	}

	return reviewed;
}

function getAnnotations(domain) {
	// TODO: get domain name instead
	var domainUri = "http://accurator.nl/" + domain + "#domain";
	return $.getJSON("/annotations", {uri:domainUri, type:"domain", enrich:"true"});
}
