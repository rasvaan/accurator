/* Accurator Register
*/

var header, frmRealName, frmUsername, frmPassword, btnRegister;

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
	header = "Create your Accurator Account";
	frmRealName = "Full name";
	frmUsername = "Username";
	frmPassword = "Password";
	btnRegister = "Register";
}

function addTextHeader() {
	$("#txtHeader").append(header);
}

function addTextForm() {
	$("#frmRealName").append(frmRealName);
	$("#frmUsername").append(frmUsername);
	$("#frmPassword").append(frmPassword);
}

function addTextButtons(register, login) {
	$("#btnRegister").append(btnRegister);
}

function registerButtonEvent() {
	$("#btnRegister").click(function() {
		document.location.href="/add_info.html";
	});	
}

function setRegisterFailureText(text) {
	alertWell = $.el.div({'class':'well'},text); 
	// clear the current
	$("#messageWell").empty();
	$("#messageWell").append(alertWell);
}