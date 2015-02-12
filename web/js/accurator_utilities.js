/* Accurator Utilities
*/
var loginWarning, loginIncomplete;

//Domain
function domainSettings(domain, onDomain) {
	if(domain === "")
		domain = "generic";
	
	$.getJSON("domain_settings", {domain:domain})
	.done(function(data){
		  onDomain(data);
	});
}


//UI
function setLinkLogo(page) {
	if(page === "profile")
	   $(".navbar-brand").attr('href', "profile.html");
	if(page === "intro")
		$(".navbar-brand").attr('href', "intro.html");
}

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
function userLoggedIn(onSuccess, onFaill) {
	//see if user is logged in
	$.getJSON("get_user")
		.done(onSuccess)
		.fail(onFail);
}

function logUserIn(onSuccess, onDismissal) {
	//make sure user is logged in
	$.getJSON("get_user")
		.done(function(data){onSuccess(data)})
		.fail(function(){loginModal(onSuccess, onDismissal)});
}

function loginModal(onSuccess, onDismissal) {
	var ui = "http://accurator.nl/ui/bird#login_modal";
	$.getJSON("ui_elements", {locale:getLocale(), ui:ui, type:"labels"})
		.done(function(data){
			loginButtonEvent(onSuccess, onDismissal);
			initModalLabels(data);
			$("#modalLogin").modal();
			$("#inputUsername").focus();
	});
}

function initModalLabels(data) {
	$("#mdlTxtTitle").html(data.mdlTxtTitle);
	$("#mdlBtnLogin").html(data.mdlBtnLogin);
	$("#mdlFrmUsername").html(data.mdlFrmUsername);
	$("#mdlFrmPassword").html(data.mdlFrmPassword);
	loginWarning = data.loginWarning;
	loginIncomplete = data.loginIncomplete;
	$("body").on('shown.bs.modal', '.modal', function () {
		$("#inputUsername").focus();
	})
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
	dataLogin = {"user":user, "password":password};
	
	$.ajax({type: "POST",
		    url: "user/login",
		    data: dataLogin,
		    success: function(data) {
				if(data.indexOf("Login failed") != -1) {
					$(".modal-body").append($.el.p({'class':'text-danger'}, loginWarning));
				} else if (data.indexOf("Login ok") != -1) {
					//Remove event listener and hide modal
					$("#modalLogin").off('hidden.bs.modal');
					$("#modalLogin").modal('hide');
					onSuccess(dataLogin);
				}
		   }
	});
}

function logout() {
	$.ajax({type: "POST", url: "user/logout"});
}

function getUserUriBase() {
	return "http://accurator.nl/user#";
}

function getUserName(userUri) {
	return userUri.replace(getUserUriBase(),"");
}

function getUserUri(userName) {
	return getUserUriBase() + userName;
}

// Navbar
function populateNavbar(userName, linkList) {
	$(".navbar-right").append(
		$.el.li({'class':'dropdown'},
			$.el.a({'href':'#',
				    'class':'dropdown-toggle',
				    'data-toggle':'dropdown',
					'role':'button',
					'aria-expanded':'false'},
					userName + " ",
					$.el.span({'class':'caret'})),
			$.el.ul({'class':'dropdown-menu',
					 'role':'menu'},
					 $.el.li($.el.a({'href':'intro.html',
									 'id':'btnLogout'},
									 "Logout")),
					 addLinks(linkList),
					 $.el.li({'class':'divider'}),
					 $.el.li($.el.a({'href':'about.html'},"About Accurator"))))
	);
	$("#btnLogout").click(function() {
		logout();
	});
}

function addLinks(linkList) {
	var links = [];
	for(var i=0; i<linkList.length; i++){
		links[i] = $.el.li($.el.a({'href':linkList[i].link},
								 linkList[i].name))
	}
	return links;
}

// Url parameters
function getParameterByName(name) {
	name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
	var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
	results = regex.exec(location.search);
	return results === null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
}
