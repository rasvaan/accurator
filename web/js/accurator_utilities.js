/* Accurator Utilities
*/
var loginWarning, loginIncomplete;

//Settings
function clearLocalStorage(setting) {
	// Remove a setting from local storage
	localStorage.removeItem(setting);
}

//Domain
function getDomain() {
	//No domain
	if(localStorage.getItem("domain") === null ||
	   localStorage.getItem("domain") === "") {
		console.log("No domain given");
		setDomainToGenericOrParameter();
	}
	var domainParameter = getParameterByName("domain");
	
	//Domain parameter
	if(!(domainParameter === "")){
		setDomain(domainParameter);
		console.log("Domain parameter set to " + domainParameter);
	}
	return localStorage.getItem("domain");
}

function setDomainToGenericOrParameter() {
	var domain = getParameterByName("domain");
	// Set domain to generic if no parameter is given
	if(domain === "") {
		setDomain("generic");
	} else {
		setDomain(domain);
	}
}

function setDomain(domain, onSuccess) {
	localStorage.setItem("domain", domain);
	save_user_info({"domain":domain}, onSuccess);
}

function domainSetting(domain) {
	$.getJSON("domains", {domain:domain})
		.done(function(data){
			return data;
	});
}

function domainSettings(domain, onDomain) {
	$.getJSON("domains", {domain:domain})
		.done(function(data){
			onDomain(data);
	});
}

function getAvailableDomains(onDomains) {
	$.getJSON("domains")
		.done(function(data){
			onDomains(data);
			return data;
	});
}

//Locale
function getLocale() {
	var paramLocale = getParameterByName("locale");
	
	if(!(paramLocale === "")) {
		setLocale(paramLocale);
	}
	if(localStorage.getItem("locale") === null ||
	   localStorage.getItem("locale") === ""){
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

function setLocale(languageCode, onSuccess) {
	localStorage.setItem("locale", languageCode);
	save_user_info({"locale":languageCode}, onSuccess);
}

//UI
function setLinkLogo(page) {
	if(page === "profile")
	   $(".navbar-brand").attr('href', "profile.html");
	if(page === "intro")
		$(".navbar-brand").attr('href', "intro.html");
}

function getUI(domainSettings, page) {
	if(typeof domainSettings != 'undefined') {
		return domainSettings.ui + page
	} else {
		return "http://accurator.nl/ui/generic#" + page;
	}
}

function alertMessage(title, text, type) {
	$("#events").prepend(
		$.el.div({'class':'row',
				  'id':'alertMessage'},
			$.el.div({'class':'col-md-12'},
				$.el.div({'class':'alert alert-' + type + ' alert-dismissible',
						  'role':'alert'},
					$.el.button({'type':'button',
								 'class':'close',
								 'data-dismiss':'alert',
								 'aria-label':'Close'},
						$.el.span({'aria-hidden':'true'},
							"x")),
					$.el.h4(title),
					$.el.p(text)))));
}

//User
function userLoggedIn(onLoggedIn, onNotLoggedIn) {
	//see if user is logged in
	$.getJSON("get_user")
		.done(onLoggedIn)
		.fail(onNotLoggedIn);
}

function logUserIn(onLoggedIn, onDismissal) {
	//make sure user is logged in
	$.getJSON("get_user")
		.done(function(data){onLoggedIn(data)})
		.fail(function(){loginModal(onLoggedIn, onDismissal)});
}

function loginModal(onSuccess, onDismissal) {
	var ui = "http://accurator.nl/ui/bird#login_modal";
	$.getJSON("ui_elements", {locale:getLocale(),
							  ui:ui,
							  type:"labels"})
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
	$("#inputUsername").keypress(function(event) {
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
					setUserSettingsLocal(dataLogin, onSuccess);
					//Remove event listener and hide modal
					$("#modalLogin").off('hidden.bs.modal');
					$("#modalLogin").modal('hide');
				}
		   }
	});
}

function logout() {
	$.ajax({type: "POST",
			url: "user/logout",
			success: function(){
				document.location.href="intro.html";
			}});
}

function getUserUriBase() {
	// Return the base of the user uri, username should be added
	return "http://accurator.nl/user#";
}

function getUserName(userUri) {
	return userUri.replace(getUserUriBase(),"");
}

function getUserUri(userName) {
	return getUserUriBase() + userName;
}

function setUserSettingsLocal(dataLogin, onSuccess){
	$.getJSON("get_user_settings")
	.done(function(data){
		localStorage.setItem("locale", data.locale);
		localStorage.setItem("domain", data.domain);
		onSuccess(dataLogin);
	});
}

function save_user_info(info, onSuccess) {
	//get the user id and post information
	if(typeof onSuccess == 'undefined')
		onSuccess = function(){console.log("No success function")};
		
	$.getJSON("get_user")
	.done(function(data){
		info.user = data.user;
		$.ajax({type: "POST",
				url: "save_user_info",
				contentType: "application/json",
				data: JSON.stringify(info),
				success: onSuccess()
		});
	});
}

// Navbar
function populateNavbar(userName, linkList) {
	$.getJSON("ui_elements", {locale:locale,
							  ui:"http://accurator.nl/ui/generic#user_dropdown",
							  type:"labels"})
		.done(function(data){
			$(".userDropdown").append(
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
							$.el.li($.el.a({'href':'#',
											'id':'btnLogout'},
											data.ddLogOut)),
							addLinks(linkList, data),
							$.el.li({'class':'divider'}),
							$.el.li($.el.a({'href':'about.html'},
											data.ddAbout))))
			)
			$("#btnLogout").click(function() {
				logout();
			});
		});
}

function addLinks(linkList, labels) {
	var links = [];

	for(i=0; i<linkList.length; i++){
		links[i] = $.el.li($.el.a({'href':linkList[i].link},
			localizedPageName(linkList, labels)
		));
	}
	return links;
}

function localizedPageName(linkList, labels) {
	if(linkList[i].name === "Profile") {
		return labels.ddProfile;
	} else {
		return linkList[i].name;
	}
}

// Url parameters
function getParameterByName(name) {
	name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
	var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
	results = regex.exec(location.search);
	return results === null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
}

// Uri
function generateIdFromUri(uri) {
	var pathArray = uri.split('/');
	return pathArray[pathArray.length - 1];
}
