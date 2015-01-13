/* Accurator Intro
*/
locale = "en";
ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#intro";
	
function introInit() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
		.done(function(data){
			registerButtonEvent();
			initLabels(data)})
		.fail(function(data, textStatus){
			$("#txtSubSlogan").replaceWith('Problem connecting to server, please contact the system administrator');});
}

function registerButtonEvent() {
	$("#btnRegister").click(function() {
		document.location.href="/register.html";
	});	
}

function initLabels(data) {
	$("#txtSlogan").prepend(data.txtSlogan);
	$("#txtSubSlogan").prepend(data.txtSubSlogan);
	$("#btnRegister").append(data.btnRegister);
	$("#btnLogin").append(data.btnLogin);
	addTextModal(data);
}

function addTextModal(data) {
	$("#mdlTxtTitle").append(data.mdlTxtTitle);
	$("#mdlBtnLogin").append(data.mdlBtnLogin);
	$("#mdlFrmUsername").append(data.mdlFrmUsername);
	$("#mdlFrmPassword").append(data.mdlFrmPassword);
}