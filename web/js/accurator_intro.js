/* Accurator Intro
*/
var locale;
var ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#intro";
var loginWarning, loginIncomplete;

function introInit() {
	locale = getLocale();
	
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
		.done(function(data){
			buttonEvents();
			initLabels(data);})
		.fail(function(data, textStatus){
			$("#txtSubSlogan").replaceWith('Problem connecting to server, please contact the system administrator');});
}

function buttonEvents() {
	$("#btnRegister").click(function() {
		document.location.href="register.html";
	});
	$("#btnLogin").click(function() {
		onSuccess = function(){
			document.location.href="profile.html";
		};
		onDismissal = function(){
			$("#modalLogin").modal('hide');
		};
		loginModal(onSuccess, onDismissal);
	});
}

function initLabels(data) {
	$("#txtSlogan").prepend(data.txtSlogan);
	$("#txtSubSlogan").prepend(data.txtSubSlogan);
	$("#btnRegister").append(data.btnRegister);
	$("#btnLogin").append(data.btnLogin);
	loginWarning = data.loginWarning;
	loginIncomplete = data.loginIncomplete;
}