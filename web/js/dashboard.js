/*******************************************************************************
Accurator Dashboard

Code for showing the dashboard page.
*******************************************************************************/
"use strict";

function dashboardInit() {
	addRow("fashion", "a lot");
}

function addRow(domain, annotations) {
	var id = "dashboardTr" + domain;
	$(".dashboardTblDomains").append(
		$.el.tr(
			{'id': id},
			$.el.td(domain),
			$.el.td(annotations)
		)
	);
	addNavigationRow(id, domain);
}

function addNavigationRow(rowId, domain) {
	$("table #" + rowId).on("click", function() {
		document.location.href = "list.html?domain=" + domain;
	});
}
