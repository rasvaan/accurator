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
			console.log(annotations);
		});
	}
}

function getAnnotations(domain) {
	// TODO: get domain name instead
	var domainUri = "http://accurator.nl/" + domain + "#domain";
	return $.getJSON("annotations", {uri:domainUri, type:"domain", enrich:"true"});
}
