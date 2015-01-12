/* Accurator Register
*/

var txtHeader, 
	frmRealName, frmUsername, frmPassword, 
	btnRegister;

function registerInit() {
	var locale = "en";
	var ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#register";
	
	$.getJSON("ui_text", {locale:locale, ui:ui})
	.done(function(data){
		registerButtonEvent();
		initVariables(data);
		addTextHeader();
		addTextForm();
		addTextButtons();})
	.fail(function(data, textStatus){
		setRegisterFailureText("Problem connecting to server, please contact the system administrator.");});
}

function initVariables(data) {
	txtHeader = data.txtHeader;
	frmRealName = data.frmRealName;
	frmUsername = data.frmUsername;
	frmPassword = data.frmPassword;
	btnRegister = data.btnRegister;
}

function addTextHeader() {
	$("#txtHeader").append(txtHeader);
}

function addTextForm() {
	$("#frmRealName").append(frmRealName);
	$("#frmUsername").append(frmUsername);
	$("#frmPassword").append(frmPassword);
}

function addTextButtons() {
	$("#btnRegister").append(btnRegister);
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