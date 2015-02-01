/* Accurator Utilities
*/
var loginWarning, loginIncomplete;

//Locale
function getLocale() {
	if(localStorage.getItem("locale") === null){
		console.log("No locale set");
		setLocaleToBrowserLanguage();
	}
	return localStorage.getItem("locale");
}

function setLocaleToBrowserLanguage() {
	var language = window.navigator.userLanguage || window.navigator.language;
	var languageCode = language.substr(0,2);
	localStorage.setItem("locale", languageCode);
}

function setLocale(languageCode) {
	localStorage.setItem("locale", languageCode);
}


//User
function userLoggedIn(onSuccess, onDismissal) {
	//get the user id
	$.getJSON("get_user")
		.done(onSuccess)
		.fail(function(){loginModal(onSuccess, onDismissal)});
}

function loginModal(onSuccess, onDismissal) {
	var ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#login_modal";
	$.getJSON("ui_elements", {locale:getLocale(), ui:ui, type:"labels"})
	.done(function(data){
		  loginButtonEvent(onSuccess, onDismissal);
		  initModalLabels(data);
		  $("#modalLogin").modal();});
}

function initModalLabels(data) {
	$("#mdlTxtTitle").html(data.mdlTxtTitle);
	$("#mdlBtnLogin").html(data.mdlBtnLogin);
	$("#mdlFrmUsername").html(data.mdlFrmUsername);
	$("#mdlFrmPassword").html(data.mdlFrmPassword);
	loginWarning = data.loginWarning;
	loginIncomplete = data.loginIncomplete;
}

function loginButtonEvent(onSuccess, onDismissal) {
	$("#mdlBtnLogin").click(function() {
		login(onSuccess);
	});
	// Login on pressing enter
	$("#inputPassword").keypress(function(event) {
		if (event.which == 13)
			login(onSuccess);
	});
	$("#modalLogin").on('hidden.bs.modal', function (e) {
		onDismissal();
	});
	$("#mdlBtnClose").click(function() {
		onDismissal();
	});
}


function login(onSuccess) {
	var user = getUserUriBase() + $("#inputUsername").val();
	var password = $("#inputPassword").val();
	
	if(user == "" || password == "") {
		$(".modal-body").append($.el.p({'class':'text-danger'}, loginIncomplete));
	} else {
		loginServer(user, password, onSuccess);
	}
}

function loginServer(user, password, onSuccess) {
	$.ajax({
		   type: "POST",
		   url: "user/login",
		   data: {"user":user, "password":password},
		   success: function(data, textStatus, request){
		   if(data.indexOf("Login failed") != -1) {
		   $(".modal-body").append($.el.p({'class':'text-danger'}, loginWarning));
		   } else if (data.indexOf("Login ok") != -1) {
		   //Remove event listener and hide modal
		   $("#modalLogin").off('hidden.bs.modal');
		   $("#modalLogin").modal('hide');
		   onSuccess();
		   }
		   }
		   });
}

function getUserUriBase() {
	return 'http://semanticweb.cs.vu.nl/user/';
}