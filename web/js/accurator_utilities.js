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

/*******************************************************************************
Settings
General setting management.
*******************************************************************************/
function clearLocalStorage(setting) {
	// remove a setting from local storage
	localStorage.removeItem(setting);
}

function setUserSettingsLocal() {
	// set user settings upon loggin in (called by loginServer)
	return $.getJSON("get_user_settings")
	.then(function(data){
		localStorage.setItem("locale", data.locale);
		localStorage.setItem("domain", data.domain);
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

function domainSettings(domain) {
	// Retrieve domain settings
	return $.getJSON("domains", {domain:domain});
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
function getLabels(locale, ui) {
	// Retrieve labels from server according to locale and ui
	return $.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"});
}

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

function setLocale(languageCode) {
	var deferred = jQuery.Deferred();

	// Action should depend on whether user is logged in
	userLoggedIn()
	.then(function() {
		localStorage.setItem("locale", languageCode);
		save_user_info({"locale":languageCode}, onSuccess);
		deferred.resolve();
	}, function() {
		localStorage.setItem("locale", languageCode);
		deferred.resolve("logged in");
	});

	return deferred.promise();
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
	$("#navbarLnkEn").click(function() {
		setLocale("en")
		.then(function() {location.reload();});
	});
	$("#navbarLnkNl").click(function() {
		setLocale("nl")
		.then(function() {
			location.reload();});
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
	if(experiment === "recommender") {
		aBArray = ["random","recommend"];
	} else {
		aBArray = ["a","b"];
	}
	var randomIndex = Math.floor(Math.random() * aBArray.length);

	// Set the A or B setting to the randomly chosen index
	setAOrB(aBArray[randomIndex]);
}

function recommenderExperiment() {
	// Settings for recommender experiment
	if(experiment === "recommender") {
		// Set interface to list view
		display.layout = "list";

		// If running an recommender experiment choose A or B
		var AOrB = getAOrB();

		if(AOrB === "recommend") {
			return true;
		} else if(AOrB === "random") {
			return false;
		}
	}
	return true;
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

function populateNavbar(userName, linkList, locale) {
	// only popluate navbar when no experiment is running
	if(typeof experiment === "undefined" || experiment === "none") {
		populateNavbarUser(userName, linkList, locale);
	} else {
		// hide recommendations button if experiment is running
		$("#btnRecommend").hide();
		$("#navbarBtnRecommend").hide();
		$("#btnResultsRecommend").hide();
		// hide search form
		$("#navbarFrmSearch").hide();
		// remove link from logo if experiment is running
		$(".navbar-brand").attr('href', "#");
	}
}

function populateNavbarUser(userName, linkList, locale) {
	// Add a user drop down based on the user and listed links
	getLabels(locale, "http://accurator.nl/ui/generic#userDropdown")
	.then(function(labels) {
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
										 	 labels.navbarLnkLogout)),
							 // Add links based on array
							 addLinks(linkList, labels),
						 	 $.el.li({'class':'divider'}),
						 	 $.el.li($.el.a({'href':'about.html'},
								 	 labels.navbarLnkAbout))))
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

function truncate(string, limit) {
	var chars;
	var i;

	chars = string.split('');
	if (chars.length > limit) {
		for (var i=chars.length - 1; i>-1; --i) {
			if (i>limit) {
				chars.length = i;
			}
			else if (' ' === chars[i]) {
				chars.length = i;
				break;
			}
		}
		chars.push('...');
	}
	return chars.join('');
}

/*******************************************************************************
User Login
User management code.
*******************************************************************************/
function userLoggedIn() {
	// see if user is logged in (random for unique request)
	return $.getJSON("get_user?time=" + Math.random())
	.then(function(user) {
		if (user.login) return user;
		if (!user.login) return jQuery.Deferred().reject(user).promise();
	});
}

function login(onSuccess, onDismissal) {
	var ui = "http://accurator.nl/ui/generic#loginModal";
	var locale = getLocale();

	getLabels(locale, ui)
	.then(function(labels) {
		// add labels and show modal
		var labels = initModalLabels(labels);
		$("#loginDivLogin").modal();
		$("#loginInpUsername").focus();
		loginButtonEvents(onSuccess, onDismissal, labels);
	})
}

function initModalLabels(data) {
	$("#loginHdrTitle").html(data.loginHdrTitle);
	$("#loginBtnLogin").html(data.loginBtnLogin);
	$("#loginLblUsername").html(data.loginLblUsername);
	$("#loginLblPassword").html(data.loginLblPassword);
	$("body").on('shown.bs.modal', '.modal', function () {
		$("#loginInpUsername").focus();
	});
	var labels = {loginTxtWarning:data.loginTxtWarning,
		 		  loginTxtIncomplete:data.loginTxtIncomplete}
	return labels;
}

function loginButtonEvents(onSuccess, onDismissal, labels) {
	// add events to login modal
	$("#loginBtnLogin").click(function() {
		processLogin(onSuccess, labels);
	});
	// login on pressing enter
	$("#loginInpPassword").keypress(function(event) {
		if (event.which == 13) processLogin(onSuccess, labels);
	});
	$("#loginInpUsername").keypress(function(event) {
		if (event.which == 13) processLogin(onSuccess, labels);
	});
	// run onDismissal if modal is dismissed
	$("#loginDivLogin").on('hidden.bs.modal', function () {
		onDismissal();
	});
	$("#loginBtnClose").click(function() {
		onDismissal();
	});
}

function processLogin(onSuccess, labels) {
	// login based upon values provided in modal
	var user = getUserUri($("#loginInpUsername").val());
	var password = $("#loginInpPassword").val();

	if(user == "" || password == "") {
		$("#loginTxtWarning").html(
			$.el.p({'class':'text-danger'}, labels.loginTxtIncomplete)
		);
	} else {
		loginServer(user, password)
		.then(function(data) {
			if(data.indexOf("Login failed") != -1) {
				// show warning login failed
				$("#loginTxtWarning").html(
					$.el.p({'class':'text-danger'}, labels.loginTxtWarning)
				);
			} else if (data.indexOf("Login ok") != -1) {
				// set user settings, hide modal and execute onSuccess
				setUserSettingsLocal()
				.then(function() {
					$("#loginDivLogin").off('hidden.bs.modal');
					$("#loginDivLogin").modal('hide');
					return userLoggedIn();
				})
				.then(function(userData) {
					onSuccess(userData);
				});
			}
		});
	}
}

function loginServer(user, password) {
	var dataLogin = {"user": user, "password": password};

	return $.ajax({type: "POST", url: "user/login", data: dataLogin});
}

function logout() {
	$.ajax({url: "user/logout", type: "POST"})
	.then(function() {
		document.location.href="intro.html";
	});
}

/*******************************************************************************
User registration
Code for registering a new user
*******************************************************************************/
function registerModal(onDismissal) {
	var ui = "http://accurator.nl/ui/generic#registerModal";
	var locale = getLocale();

	getLabels(locale, ui)
	.then(function(data) {
		var labels = initRegisterModalLabels(data);
		registerButtonEvent(onDismissal, labels);

		$("#registerDivRegister").modal();
		$("#registerInpFullName").focus();
	});
}

function initRegisterModalLabels(data) {
	// add retrieved labels to html elements
	$("#registerHdrTitle").html(data.registerHdrTitle);
	$("#registerLblFullName").html(data.registerLblFullName);
	$("#registerLblUsername").html(data.registerLblUsername);
	$("#registerLblPassword").html(data.registerLblPassword);
	$("#registerLblPasswordRepeat").html(data.registerLblPasswordRepeat);
	$("#registerBtnRegister").html(data.registerBtnRegister);

	// set text variables for possible later use
	var labels = {
		registerTxtRegistrationFailed: data.registerTxtRegistrationFailed,
		registerTxtUsernameFail: data.registerTxtUsernameFail,
		registerTxtPasswordsMatchFail: data.registerTxtPasswordsMatchFail,
		registerTxtUserTaken: data.registerTxtUserTaken,
		registerTxtServerError: data.registerTxtServerError
	};

	$("body").on('shown.bs.modal', '.modal', function () {
		$("#registerInpFullName").focus();
	})

	return labels;
}

function registerButtonEvent(onDismissal, labels) {
	$("#registerBtnRegister").click(function() {
		register(labels);
	});
	// register on pressing enter
	$("#registerInpPasswordRepeat").keypress(function(event) {
		if (event.which == 13) {
			register(labels);
		}
	});
	$("#registerDivRegister").on('hidden.bs.modal', function (e) {
		onDismissal();
	});
	$("#registerBtnClose").click(function() {
		onDismissal();
	});
}

function register(labels) {
	// get and check initial form input
	var name = $("#registerInpFullName").val();
	var user = $("#registerInpUsername").val();
	var userUri = getUserUri(user);
	var password = $("#registerInpPassword").val();
	var passwordRepeat = $("#registerInpPasswordRepeat").val();

	if((name == "") || (user == "") || (password == "") || (passwordRepeat == "")){
		setRegisterFailureText(labels.registerTxtRegistrationFailed);
	} else if (checkUsername(user)) {
		setRegisterFailureText(labels.registerTxtUsernameFail);
	} else if (password != passwordRepeat){
		setRegisterFailureText(labels.registerTxtPasswordsMatchFail);
	} else {
		// Attempt registration
		registerServer(name, userUri, password, labels);
	}
}

function setRegisterFailureText(text) {
	var alertMessage = $.el.div({'class':'registerMessage'},
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

function registerServer(name, user, password, labels) {
	var json = {"name":name, "user":user, "password":password};

	$.ajax({
		type: "POST",
		url: "register_user",
		contentType: "application/json",
		data: JSON.stringify(json),
	}).then(function() {
		// we are sometimes doing research you know
		if(experiment !== "none")
			flipAOrB();
		// login user upon succesful register
		firstLogin(user, password);
	}, function(response, textStatus, errorThrown) {
		if(errorThrown == "Not Found")
			setRegisterFailureText("Server did not respond.");
		if(response.responseText.indexOf("User already exists") > -1) {
			setRegisterFailureText(labels.registerTxtUserTaken);
		} else {
			setRegisterFailureText(labels.registerTxtServerError);
		}
	});
}

function firstLogin(user, password) {
	// loginServer from utilities is not used because it resets settings upon
	// retrieving non existent settings from user.db (hence, firstLogin)
	$.ajax({
		type: "POST",
		url: "user/login",
		data: {"user":user, "password":password},
	})
	.then(function(data) {
		// save the locale and domain currently in local storage
		save_user_info({"locale": locale, "domain": domain}, function() {
			// determine which page will be shown next
			if(experiment === "true") {
				document.location.href="form.html";
			} else {
				document.location.href="domain.html";
			}
		 });
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
