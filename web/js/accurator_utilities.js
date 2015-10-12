/*******************************************************************************
Accurator Utilities
Functions used by multiple javascript files. Topics include:

- Settings
- Annotation domain
- Locale
- Experiment
- User interface
- User management
- Uri
*******************************************************************************/

var loginWarning, loginIncomplete;

/*******************************************************************************
Settings
General setting management.
*******************************************************************************/
function clearLocalStorage(setting) {
	// remove a setting from local storage
	localStorage.removeItem(setting);
}

function setUserSettingsLocal(dataLogin, onSuccess){
	// set user settings upon loggin in (called by loginServer)
	$.getJSON("get_user_settings")
	.done(function(data){
		localStorage.setItem("locale", data.locale);
		localStorage.setItem("domain", data.domain);
		onSuccess(dataLogin);
	});
}

function save_user_info(info, onSuccess) {
	// save user settings to user.db of Cliopatria

	if(typeof onSuccess == 'undefined')
		onSuccess = function(){};

	// get the user id and post information
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

function getParameterByName(name) {
	// retrieve information from url parameters (often settings)
	name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
	var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
	results = regex.exec(location.search);
	return results === null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
}

/*******************************************************************************
Annotation Domain
Functionallity to change the annotation domain (e.g. bird or bible). Functions
include the retrieval and storage of the domain setting and the information
related to the domain (e.g. taxonomies, instances, illustrative image url)
*******************************************************************************/
function getDomain() {
	//No domain
	if(localStorage.getItem("domain") === null ||
	   localStorage.getItem("domain") === "") {
		setDomainToGenericOrParameter();
	}
	var domainParameter = getParameterByName("domain");

	//Domain parameter
	if(!(domainParameter === "")){
		setDomain(domainParameter);
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

/*******************************************************************************
Locale
Functionallity to addapt to the desired locale.
*******************************************************************************/
function getLocale() {
	// check url for locale parameter
	var paramLocale = getParameterByName("locale");

	// if parameter is given, set locale accordingly
	if(!(paramLocale === "")) {
		setLocale(paramLocale);
	}
	// if there is no locale in local storage, set according to browser language
	if(localStorage.getItem("locale") === null ||
	   localStorage.getItem("locale") === ""){
		setLocaleToBrowserLanguage();
	}
	return localStorage.getItem("locale");
}

function setLocaleToBrowserLanguage() {
	// retrieve locale from browser
	var language = window.navigator.userLanguage || window.navigator.language;
	var languageCode = language.substr(0,2);

	// Save locale to localStorage and user.db
	localStorage.setItem("locale", languageCode);
	var onSuccess = function(){};
	save_user_info({"locale":languageCode}, onSuccess);
}

function setLocale(languageCode, onSuccess) {
	// Action should depend on whether user is logged in
	var onLoggedIn = function() {
		localStorage.setItem("locale", languageCode);
		save_user_info({"locale":languageCode}, onSuccess);
	};
	var onNotLoggedIn = function() {
		localStorage.setItem("locale", languageCode);
		onSuccess();
	};

	userLoggedIn(onLoggedIn, onNotLoggedIn);
}

function populateFlags(locale) {
	// Code to add flags to navbar allowing to change the locale
	$(".flagDropdown").append(
		$.el.li({'class':'dropdown'},
				 getInitialFlag(locale),
			$.el.ul({'class':'dropdown-menu',
					'role':'menu'},
					$.el.li($.el.a({'href':'#',
									'id':'flagEn'},
									$.el.span({'class':'flag-icon flag-icon-en'}),
									" English")),
					$.el.li($.el.a({'href':'#',
									'id':'flagNl'},
									$.el.span({'class':'flag-icon flag-icon-nl'}),
									" Nederlands"))
			)
		)
	);
	// Add flag events
	flagEvents();
}

function flagEvents() {
	var onSuccess = function(){location.reload();};

	$("#flagEn").click(function() {
		setLocale("en", onSuccess);
	});
	$("#flagNl").click(function() {
		setLocale("nl", onSuccess);
	});
}

function getInitialFlag(locale) {
	// set the flag to be shown in the navbar
	return $.el.a({'href':'#',
				   'class':'dropdown-toggle',
				   'data-toggle':'dropdown',
				   'role':'button'},
				   $.el.span({'class':'flag-icon flag-icon-' + locale}),
				   " ",
				   $.el.span({'class':'caret'})
	)
}

/*******************************************************************************
Experiment
Functionallity for running an experiment with Accurator. Settings regarding
which experiment and whether we are running setting A or B can be retrieved.
*******************************************************************************/
function getExperiment() {
	// get experiment url parameter
	var experimentParameter = getParameterByName("experiment");

	// set experiment to url parameter or none if empty parameter and localStorage
	if(!(experimentParameter === "")) {
		// set experiment setting to parameter if available
		localStorage.setItem("experiment", experimentParameter);
	} else if(localStorage.getItem("experiment") === null ||
			  localStorage.getItem("experiment") === "") {
		// if no parameter set to none and return value
		localStorage.setItem("experiment", "none");
	}
	return localStorage.getItem("experiment");
}

function getAOrB() {
	// return the a or b setting from local storage
	return localStorage.getItem("ab");
}

function setAOrB(ab) {
	// set the a or b setting in local storage
	localStorage.setItem("ab", ab);
}

/*******************************************************************************
User Interface
Functionallity making the ui adapt
*******************************************************************************/
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

function populateNavbar(userName, linkList) {
	// Only popluate navbar when no experiment is running
	if(typeof experiment === "undefined" || experiment === "none") {
		populateUserDropdown(userName, linkList);
	} else {
		// Hide recommendations button if experiment is running
		$("#btnRecommend").hide();
		$("#btnAnnotateRecommend").hide();
		$("#btnResultsRecommend").hide();
		// Hide search form
		$("#frmGroupSearch").hide();
		// Remove link from logo if experiment is running
		$(".navbar-brand").attr('href', "#");
	}
}

function populateUserDropdown(userName, linkList) {
	// Add a user drop down based on the user and listed links
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
							 // Add links based on array
							 addLinks(linkList, data),
						 	 $.el.li({'class':'divider'}),
						 	 $.el.li($.el.a({'href':'about.html'},
								 	 data.ddAbout))))
		)
		// Add logout event to menu item
		$("#btnLogout").click(function() {
			logout();
		});
	});
}

function addLinks(linkList, labels) {
	var links = [];

	// Populate the list of additional links in the navbar dropdown
	for(var i=0; i<linkList.length; i++){
		links[i] = $.el.li($.el.a({'href':linkList[i].link},
			localizedPageName(linkList, labels, i)
		));
	}
	return links;
}

function localizedPageName(linkList, labels, counter) {
	if(linkList[counter].name === "Profile") {
		return labels.ddProfile;
	} else {
		return linkList[counter].name;
	}
}

function setLinkLogo(page) {
	if(page === "profile")
	   $(".navbar-brand").attr('href', "profile.html");
	if(page === "intro")
		$(".navbar-brand").attr('href', "intro.html");
}

/*******************************************************************************
User
User management code.
*******************************************************************************/
function userLoggedIn(onLoggedIn, onNotLoggedIn) {
	//see if user is logged in (random for unique request)
	$.getJSON("get_user?time=" + Math.random())
		.done(onLoggedIn)
		.fail(onNotLoggedIn);
}

function logUserIn(onLoggedIn, onDismissal) {
	//make sure user is logged in (random for unique request)
	$.getJSON("get_user?time=" + Math.random())
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
	var user = getUserUri($("#inputUsername").val());
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
					// remove event listener and hide modal
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

/*******************************************************************************
Uri
Code for working with Uris
*******************************************************************************/
function getUserUri(userName) {
	// concatenate proper user uri (e.g. )
	return getUserUriBase() + userName.toLowerCase();
}

function getUserName(userUri) {
	// retrieve user name from uri
	return userUri.replace(getUserUriBase(),"");
}

function getUserUriBase() {
	// return the base of the user uri, username should be added
	return "http://accurator.nl/user#";
}

function generateIdFromUri(uri) {
	// create a html id from a uri (jquery doesn't play well with full uri's)
	var pathArray = uri.split('/');
	return pathArray[pathArray.length - 1];
}
