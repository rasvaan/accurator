/* Accurator Intro
*/
var locale;
var ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#intro";
var loginWarning, loginIncomplete;

function introInit() {
	locale = getLocale();
	
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
		.done(function(data){
			registerButtonEvent();
			loginButtonEvent(data);
			initLabels(data);})
		.fail(function(data, textStatus){
			$("#txtSubSlogan").replaceWith('Problem connecting to server, please contact the system administrator');});
}

function registerButtonEvent() {
	$("#btnRegister").click(function() {
		document.location.href="register.html";
	});	
}

function loginButtonEvent(data) {
	$("#mdlBtnLogin").click(function() {
		login();
	});
	// Login on pressing enter
	$("#inputPassword").keypress(function(event) {
		if (event.which == 13) {
			login();
		}});
}

function login() {
	var user = getUserUriBase() + $("#inputUsername").val();
	var password = $("#inputPassword").val();
	
	if(user == "" || password == "") {
		$(".modal-body").append($.el.p({'class':'text-danger'}, loginIncomplete));
	} else {
		loginServer(user, password);
	}
}

function loginServer(user, password) {
	var json = {"user":user, "password":password};
	
	$.ajax({
		type: "POST",
		url: "user_login",
		contentType: "application/json",
		data: JSON.stringify(json),
		success: function(data, textStatus, request){
			document.location.href="additional_info.html";
		},
		error: function (request, textStatus, errorThrown) {
			$(".modal-body").append($.el.p({'class':'text-danger'}, loginWarning));
		}
	});
}

function initLabels(data) {
	$("#txtSlogan").prepend(data.txtSlogan);
	$("#txtSubSlogan").prepend(data.txtSubSlogan);
	$("#btnRegister").append(data.btnRegister);
	$("#btnLogin").append(data.btnLogin);
	addTextModal(data);
	loginWarning = data.loginWarning;
	loginIncomplete = data.loginIncomplete;
}

function addTextModal(data) {
	$("#mdlTxtTitle").append(data.mdlTxtTitle);
	$("#mdlBtnLogin").append(data.mdlBtnLogin);
	$("#mdlFrmUsername").append(data.mdlFrmUsername);
	$("#mdlFrmPassword").append(data.mdlFrmPassword);
}