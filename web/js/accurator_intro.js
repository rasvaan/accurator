/* Accurator Intro
*/

var txtSlogan, txtSubSlogan, 
	btnRegister, btnLogin,
	mdlTxtTitle, mdlFrmUsername, mdlFrmPassword,
	mdlBtnLogin;
	
function introInit() {
	locale = "en";
	ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#intro";
	$.getJSON("ui_text", {locale:locale, ui:ui})
		.done(function(data){
			registerButtonEvent();
			initVariables(data);
			addTextSlogan();
			addTextButton();
			addTextModal();})
		.fail(function(data, textStatus){
			$("#txtSubSlogan").replaceWith('Problem connecting to server, please contact the system administrator');});
}

function initVariables(data) {
	txtSlogan = data.txtSlogan;
	txtSubSlogan = data.txtSubSlogan;
	btnRegister = data.btnRegister;
	btnLogin = data.btnLogin;
	mdlTxtTitle = data.mdlTxtTitle;
	mdlBtnLogin = data.mdlBtnLogin;
	mdlFrmUsername = data.mdlFrmUsername;
	mdlFrmPassword = data.mdlFrmPassword;
}

function addTextSlogan() {
	$("#txtSlogan").prepend(txtSlogan);
	$("#txtSubSlogan").prepend(txtSubSlogan);
}

function addTextButton() {
	$("#btnRegister").append(btnRegister);
	$("#btnLogin").append(btnLogin);
}

function addTextModal() {
	$("#mdlTxtTitle").append(mdlTxtTitle);
	$("#mdlBtnLogin").append(mdlBtnLogin);
	$("#mdlFrmUsername").append(mdlFrmUsername);
	$("#mdlFrmPassword").append(mdlFrmPassword);
}

function registerButtonEvent() {
	$("#btnRegister").click(function() {
		document.location.href="/register.html";
	});	
}