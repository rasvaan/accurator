/*******************************************************************************
Accurator Annotations

Code for showing the annotations.
*******************************************************************************/
"use strict";

function annotationsInit() {
	var annotations;
	var domain = getParameterByName("domain");

	clearLocalStorage("annotations"); // will be generating new list of annotations

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
		.then(function(annotations) {
			// add annotations to local storage
			localStorage.setItem("annotations", JSON.stringify(annotations));

			// add annotations to the interface
			for (var i=0; i<annotations.length; i++) {
				addRow(annotations[i]);
			}
		});
	}
}

function addRow(annotation) {
	var rowId = 'annotationsTr' + generateIdFromUri(annotation.uri);
	var buttons = new ReviewButtons(annotation.uri, rowId, "sm");
	var objectTitle = normalizeTitle(annotation.object);

	$(".annotationsTblAnnotations").append(
		$.el.tr(
			{'id':rowId},
			$.el.td(
				annotation.label
			),
			$.el.td(
				annotation.type
			),
			$.el.td(
				$.el.a(
					{'href': "/review/object.html?uri=" + annotation.object.uri},
					objectTitle
				)
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

function getAnnotations(domain) {
	// TODO: get domain name instead
	var domainUri = "http://accurator.nl/" + domain + "#domain";
	return $.getJSON("/annotations", {uri:domainUri, type:"domain", enrich:"true"});
}
