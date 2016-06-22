/*******************************************************************************
Accurator Annotations

Code for showing the annotations.
*******************************************************************************/
"use strict";

function annotationsInit() {
	var domain = getParameterByName("domain");

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
			for (var i=0; i<annotations.length; i++) {
				addRow(annotations[i]);
			}
		});
	}
}

function addRow(annotation) {

	$(".annotationsTblAnnotations").append(
		$.el.tr(
			$.el.td(
				$.el.a(
					{'href': "review.html?uri=" + annotation.uri},
					annotation.label
			)),
			$.el.td(
				$.el.a(
					{'href': "review.html?uri=" + annotation.object.uri},
					annotation.object.title
			)),
			$.el.td(
				$.el.a(
					{'href': "review.html?uri=" + annotation.annotator},
					getUserName(annotation.annotator)
			))
	));
}

function getAnnotations(domain) {
	// TODO: get domain name instead
	var domainUri = "http://accurator.nl/" + domain + "#domain";
	return $.getJSON("annotations", {uri:domainUri, type:"domain", enrich:"true"});
}
