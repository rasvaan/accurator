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
			addRow(data[1]);
			// for (var i=0; i<data.length; i++) {
			// 	addRow(data[i]);
			// }
		});
	}
}

function addRow(domain) {
	var buttonId = "dashboardBtn" + domain;

	// add request for statistics
	getDomainStatistics(domain)
	.then(function(statistics) {
		$(".dashboardTblDomains").append(
			$.el.tr(
				$.el.td(
					$.el.a(
						{'href': "list.html?domain=" + domain},
						domain
				)),
				$.el.td(statistics.annotators),
				$.el.td(statistics.objects_annotated),
				$.el.td(statistics.number_annotations),
				$.el.td(statistics.reviewed_annotations),
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
	}, function() {
		console.log("no statistics");
	});
}

function addButtonEvents(buttonId, domain) {
	$("table #" + buttonId + "Csv").on("click", function() {
		alert("donwload csv of ", domain);
	});
	$("table #" + buttonId + "Rdf").on("click", function() {
		alert("donwload rdf of ", domain);
	});
}
