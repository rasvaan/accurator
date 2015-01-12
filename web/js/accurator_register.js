/* Accurator Register
*/

function registerInit() {
	var locale = "en";
	var ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#register";
	
	$.getJSON("ui_text", {locale:locale, ui:ui})
	.done(function(data){
		registerButtonEvent();
		initLabels(data);})
	.fail(function(data, textStatus){
		setRegisterFailureText("Problem connecting to server, please contact the system administrator.");});
}

function initLabels(data) {
	$("#txtHeader").append(data.txtHeader);
	$("#frmRealName").append(data.frmRealName);
	$("#frmUsername").append(data.frmUsername);
	$("#frmPassword").append(data.frmPassword);
	$("#btnRegister").append(data.btnRegister);
}

function registerButtonEvent() {
	$("#btnRegister").click(function() {
		document.location.href="/additional_info.html";
	});	
}

function setRegisterFailureText(text) {
	alertWell = $.el.div({'class':'well'},text); 
	// clear the current
	$("#messageWell").empty();
	$("#messageWell").append(alertWell);
}