/*******************************************************************************
Accurator Annotations

Code for showing the annotations.
*******************************************************************************/
"use strict";

function annotationsInit() {
	var objects, annotations;
	var domain = getParameterByName("domain");
	var show = getParameterByName("show");
	if (!show) show = "annotations";

	clearLocalStorage("annotations"); // will be generating new list of annotations

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
		// add title
		$("#annotationsHdrSlogan").prepend("Annotations " + domain + " domain");

		getAnnotations(domain)
		.then(function(annotationData) {
			annotations = annotationData;
			objects = objectAnnotations(annotations);

			// add annotations to local storage
			localStorage.setItem("annotations", JSON.stringify(annotations));

			// add annotations to the interface
			for (var i=0; i<annotations.length; i++) {
				addRow(annotations[i]);
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

function addRow(annotation) {
	var rowId = 'annotationsTr' + generateIdFromUri(annotation.uri);
	var buttons = new ReviewButtons(annotation.uri, rowId, "sm");

	$(".annotationsTblAnnotations").append(
		$.el.tr(
			{'id':rowId},
			$.el.td(
				$.el.a(
					{'href': "review/annotation.html?uri=" + annotation.uri},
					annotation.label
			)),
			$.el.td(
				annotation.object.title
			),
			// 	$.el.a(
			// 		{'href': "review.html?uri=" + annotation.object.uri},
			// 		annotation.object.title
			// )),
			$.el.td(
				getUserName(annotation.annotator)
			),
			$.el.td(
				buttons.node
			)
			// 	$.el.a(
			// 		{'href': "review.html?uri=" + annotation.annotator},
			// 		getUserName(annotation.annotator)
			// ))
	));

	colorRow(rowId, annotation);
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

function getAnnotations(domain) {
	// TODO: get domain name instead
	var domainUri = "http://accurator.nl/" + domain + "#domain";
	return $.getJSON("/annotations", {uri:domainUri, type:"domain", enrich:"true"});
}
