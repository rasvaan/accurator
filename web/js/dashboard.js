/*******************************************************************************
Accurator Dashboard

Code for showing the dashboard page.
*******************************************************************************/
"use strict";

function dashboardInit() {
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
		getAvailableDomains()
		.then(function(data) {
			for (var i=0; i<data.length; i++) {
				addRow(data[i], "a lot", "not many");
			}
		});
	}
}

function addRow(domain, annotations, reviewed) {
	var buttonId = "dashboardBtn" + domain;

	$(".dashboardTblDomains").append(
		$.el.tr(
			$.el.td(
				$.el.a(
					{'href': "list.html?domain=" + domain},
					domain
			)),
			$.el.td(annotations),
			$.el.td(reviewed),
			$.el.td(
				$.el.button(
					{'class':'btn btn-primary btn-xs dashboardBtnDownload',
					 'id':buttonId + "Csv"},
					"csv"),
				$.el.button(
					{'class':'btn btn-success btn-xs dashboardBtnDownload',
					 'id':buttonId + "Rdf"},
					"rdf")
			)
		)
	);
	addButtonEvents(buttonId, domain);
}

function addButtonEvents(buttonId, domain) {
	$("table #" + buttonId + "Csv").on("click", function() {
		alert("donwload csv of ", domain);
	});
	$("table #" + buttonId + "Rdf").on("click", function() {
		alert("donwload rdf of ", domain);
	});
}
