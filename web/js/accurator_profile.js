/* Accurator Profile
*/
var user = "rasvaan";
var locale = "en";
var ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#profile";
	
function profileInit() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
		.done(function(data){
			registerButtonEvent();
			initLabels(data);})
		.fail(function(data, textStatus){
			$("#txtSubSlogan").replaceWith('Problem connecting to server, please contact the system administrator');});
}

function initLabels(data) {
	$("#txtSlogan").prepend(data.txtSlogan + " " + user);
	$("#txtSubSlogan").prepend(data.txtSubSlogan);
	$("#btnRecommend").append(data.btnRecommend);
	$("#btnSearch").append(data.btnSearch);
	$("#lblLastAnnotated").append(data.lblLastAnnotated);
}

function registerButtonEvent() {
	$("#btnRecommend").click(function() {
		document.location.href="/recommendations.html";
	});	
}