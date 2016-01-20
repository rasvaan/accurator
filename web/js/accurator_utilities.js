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

var loginTxtWarning, loginTxtIncomplete;

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
Functionality to change the annotation domain (e.g. bird or bible). Functions
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
Locale and language flags
Functionallity to adapt to the desired locale.
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
	$(".navbarLstFlag").append(
		$.el.li({'class':'dropdown'},
				 getInitialFlag(locale),
			$.el.ul({'class':'dropdown-menu',
					'role':'menu'},
					$.el.li($.el.a({'href':'#',
									'id':'navbarLnkEn'},
									$.el.span({'class':'flag-icon flag-icon-en'}),
									" English")),
					$.el.li($.el.a({'href':'#',
									'id':'navbarLnkNl'},
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

	$("#navbarLnkEn").click(function() {
		setLocale("en", onSuccess);
	});
	$("#navbarLnkNl").click(function() {
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

function flipAOrB() {
	var aBArray = [];

	// Get an array with A or B for the specified experiment
	if(experiment === "true") {
		aBArray = ["random","recommend"];
	} else {
		aBArray = ["a","b"];
	}
	var randomIndex = Math.floor(Math.random() * aBArray.length);

	// Set the A or B setting to the randomly chosen index
	setAOrB(aBArray[randomIndex]);
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
	$("#eventsDiv").prepend(
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
		populateNavbarUser(userName, linkList);
	} else {
		// Hide recommendations button if experiment is running
		$("#btnRecommend").hide();
		$("#navbarBtnRecommend").hide();
		$("#btnResultsRecommend").hide();
		// Hide search form
		$("#navbarFrmSearch").hide();
		// Remove link from logo if experiment is running
		$(".navbar-brand").attr('href', "#");
	}
}

function populateNavbarUser(userName, linkList) {
	// Add a user drop down based on the user and listed links
	$.getJSON("ui_elements", {locale:locale,
							  ui:"http://accurator.nl/ui/generic#userDropdown",
							  type:"labels"})
	.done(function(data){
		$(".navbarLstUser").append(
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
								         	 'id':'navbarLnkLogout'},
										 	 data.navbarLnkLogout)),
							 // Add links based on array
							 addLinks(linkList, data),
						 	 $.el.li({'class':'divider'}),
						 	 $.el.li($.el.a({'href':'about.html'},
								 	 data.navbarLnkAbout))))
		)
		// Add logout event to menu item
		$("#navbarLnkLogout").click(function() {
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
		return labels.navbarLblProfile;
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
User Login
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
	var ui = "http://accurator.nl/ui/generic#loginModal";
	$.getJSON("ui_elements", {locale:getLocale(),
							  ui:ui,
							  type:"labels"})
		.done(function(data){
			loginButtonEvent(onSuccess, onDismissal);
			initModalLabels(data);
			$("#loginDivLogin").modal();
			$("#loginInpUsername").focus();
	});
}

function initModalLabels(data) {
	$("#loginHdrTitle").html(data.loginHdrTitle);
	$("#loginBtnLogin").html(data.loginBtnLogin);
	$("#loginLblUsername").html(data.loginLblUsername);
	$("#loginLblPassword").html(data.loginLblPassword);
	loginTxtWarning = data.loginTxtWarning;
	loginTxtIncomplete = data.loginTxtIncomplete;
	$("body").on('shown.bs.modal', '.modal', function () {
		$("#loginInpUsername").focus();
	})
}

function loginButtonEvent(onSuccess, onDismissal) {
	$("#loginBtnLogin").click(function() {
		login(onSuccess);
	});
	// Login on pressing enter
	$("#loginInpPassword").keypress(function(event) {
		if (event.which == 13)
			login(onSuccess);
	});
	$("#loginInpUsername").keypress(function(event) {
		if (event.which == 13)
			login(onSuccess);
	});
	$("#loginDivLogin").on('hidden.bs.modal', function (e) {
		onDismissal();
	});
	$("#loginBtnClose").click(function() {
		onDismissal();
	});
}

function login(onSuccess) {
	var user = getUserUri($("#loginInpUsername").val());
	var password = $("#loginInpPassword").val();

	if(user == "" || password == "") {
		$(".modal-body").append($.el.p({'class':'text-danger'}, loginTxtIncomplete));
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
					$(".modal-body").append($.el.p({'class':'text-danger'}, loginTxtWarning));
				} else if (data.indexOf("Login ok") != -1) {

					setUserSettingsLocal(dataLogin, onSuccess);
					// remove event listener and hide modal
					$("#loginDivLogin").off('hidden.bs.modal');
					$("#loginDivLogin").modal('hide');
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
User registration
Code for registering a new user
*******************************************************************************/
function registerModal(onDismissal) {
	var ui = "http://accurator.nl/ui/generic#registerModal";
	$.getJSON("ui_elements", {locale:getLocale(),
							  ui:ui,
							  type:"labels"})
		.done(function(data){
			registerButtonEvent(onDismissal);
			initRegisterModalLabels(data);
			$("#registerDivRegister").modal();
			$("#registerInpFullName").focus();
	});
}

function initRegisterModalLabels(labels) {
	// Add retrieved labels to html elements
	$("#registerHdrTitle").html(labels.registerHdrTitle);
	$("#registerLblFullName").html(labels.registerLblFullName);
	$("#registerLblUsername").html(labels.registerLblUsername);
	$("#registerLblPassword").html(labels.registerLblPassword);
	$("#registerLblPasswordRepeat").html(labels.registerLblPasswordRepeat);
	$("#registerBtnRegister").html(labels.registerBtnRegister);

	// Set text variables for possible later use
	registerTxtRegistrationFailed = labels.registerTxtRegistrationFailed;
	registerTxtUsernameFail = labels.registerTxtUsernameFail;
	registerTxtPasswordsMatchFail = labels.registerTxtPasswordsMatchFail;
	registerTxtUserTaken = labels.registerTxtUserTaken;
	registerTxtServerError = labels.registerTxtServerError;
	$("body").on('shown.bs.modal', '.modal', function () {
		$("#registerInpFullName").focus();
	})
}

function registerButtonEvent(onDismissal) {
	$("#registerBtnRegister").click(function() {
		register();
	});
	// register on pressing enter
	$("#registerInpPasswordRepeat").keypress(function(event) {
		if (event.which == 13) {
			register();
		}
	});
	$("#registerDivRegister").on('hidden.bs.modal', function (e) {
		onDismissal();
	});
	$("#registerBtnClose").click(function() {
		onDismissal();
	});
}

function register() {
	// Get and check initial form input
	var name = $("#registerInpFullName").val();
	var user = $("#registerInpUsername").val();
	var userUri = getUserUri(user);
	var password = $("#registerInpPassword").val();
	var passwordRepeat = $("#registerInpPasswordRepeat").val();

	if((name == "") || (user == "") || (password == "") || (passwordRepeat == "")){
		setRegisterFailureText(registerTxtRegistrationFailed);
	} else if (checkUsername(user)) {
		setRegisterFailureText(registerTxtUsernameFail);
	} else if (password != passwordRepeat){
		setRegisterFailureText(registerTxtPasswordsMatchFail);
	} else {
		// Attempt registration
		registerServer(name, userUri, password);
	}
}

function setRegisterFailureText(text) {
	alertMessage = $.el.div({'class':'registerMessage'},
			$.el.h5({'class':'text-danger'}, text));
	// clear the current
	$("#registerTxtWarning").empty();
	$("#registerTxtWarning").append(alertMessage);
}

function checkUsername(user) {
	// Only allow letters, numbers and underscores in username
	var illegalChars = /\W/;

	if (illegalChars.test(user)) {
		return true;
	} else {
		return false;
	}
}

function registerServer(name, user, password) {
	var json = {"name":name, "user":user, "password":password};

	$.ajax({
		type: "POST",
		url: "register_user",
		contentType: "application/json",
		data: JSON.stringify(json),
		success: function(){
			// We are sometimes doing research you know
			if(experiment !== "none")
				flipAOrB();
			// Login user upon succesful register
			firstLogin(user, password);
		},
		error: function (request, textStatus, errorThrown) {
			if(errorThrown == "Not Found")
	        	setRegisterFailureText("Server did not respond.");
	        if(request.responseText.indexOf("User already exists") > -1) {
	    		setRegisterFailureText(registerTxtUserTaken);
	        } else {
	        	setRegisterFailureText(registerTxtServerError);
	        }
		}
	});
}

function firstLogin(user, password) {
	// loginServer from utilities is not used because it resets settings upon
	// retrieving non existent settings from user.db (hence, firstLogin)
	$.ajax({type: "POST",
			url: "user/login",
			data: {"user":user, "password":password},
			success: function(data) {
				// Save the locale and domain currently in local storage
				save_user_info({"locale":locale,"domain":domain}, function(){
					// Determine which page will be shown next
					if(experiment === "true") {
						document.location.href="form.html";
					} else {
						document.location.href="domain.html";
					}
				 });
		   }
	});
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
	var pathArray = uri.split(/[/#]/);
	return pathArray[pathArray.length - 1];
}
