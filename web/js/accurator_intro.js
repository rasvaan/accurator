/* Accurator Intro
*/
var locale;
var ui = "http://accurator.nl/ui/bird#intro";

function introInit() {
	// If user is logged in go to profile page otherwise show intro.
	onSuccess = function() {
		document.location.href="profile.html";
	};
	onFail = function() {
		locale = getLocale();
		populateUI();
	};
	userLoggedIn(onSuccess, onFail);
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
		.done(function(data){
			  addButtonEvents();
			  initLabels(data);})
		.fail(function(data, textStatus){
			  $("#txtSubSlogan").replaceWith('Problem connecting to server, please contact the system administrator');});
}

function addButtonEvents() {
	$("#btnRegister").click(function() {
		document.location.href="register.html";
	});
	$("#btnLogin").click(function() {
		onSuccess = function() {
			document.location.href="profile.html";
		};
		onDismissal = function() {
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
	$("#lnkAbout").append(data.lnkAbout);
}