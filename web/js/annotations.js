/*******************************************************************************
Accurator Annotations

Code for showing the annotations.
*******************************************************************************/
"use strict";

function annotationsInit() {
	var domain = getParameterByName("domain");
	clearLocalStorage("annotations"); // will be generating new list of annotations

	// TODO: change to check admin priviliges
	userLoggedIn()
	.then(function() {
		// user is logged in as admin, so draw page
		drawPage();
	}, function() {
		// user is not logged in as admin, show modal
		var onDismissal = function() {document.location.href = "intro.html";};
		login(drawPage, onDismissal);
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
			$("#" + rowId).attr('class', 'success');
		} else if (annotation.reviews[0].judgement === "unsure") {
			$("#" + rowId).attr('class', 'warning');
		} else if (annotation.reviews[0].judgement === "disagree") {
			$("#" + rowId).attr('class', 'danger');
		}
	}
}

function getAnnotations(domain) {
	// TODO: get domain name instead
	var domainUri = "http://accurator.nl/" + domain + "#domain";
	return $.getJSON("annotations", {uri:domainUri, type:"domain", enrich:"true"});
}
