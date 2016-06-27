/*******************************************************************************
Accurator Review

Code for page allowing the review of annotations.
*******************************************************************************/
"use strict";

function reviewInit() {
	console.log("local: ",localStorage.getItem("annotations"));

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
		addNavigation();
	}
}

function addNavigation() {
	console.log("wire the buttons");
}
