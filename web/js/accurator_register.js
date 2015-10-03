/*******************************************************************************
Accurator Register
Code for registering new user. After registering
*******************************************************************************/
var locale, domain, experiment, ui;
var lblRegistrationFailed, lblUsernameFail, lblPasswordsMatchFail, lblUserTaken, lblServerError;

function registerInit() {
	// Get settings
	locale = getLocale();
	domain = getDomain();
	experiment = getExperiment();

	// Add language switch to navbar
	populateFlags(locale);

	var onDomain = function(domainSettings) {
		ui = getUI(domainSettings, "register");
		populateUI();
	}
	domainSettings = domainSettings(domain, onDomain);
}

function nextPage() {
	// Determine which page will be shown next
	if(experiment === "true") {
		return function(){document.location.href="additional_info.html"};
	} else {
		return function(){document.location.href="domain.html"};
	}
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
	.done(function(labels) {
		// Add events triggering the register attempt
		registerEvent();
		initLabels(labels);
		$("#frmRealName").focus();});
}

function initLabels(labels) {
	// Add retrieved labels to html elements
	document.title = labels.title;
	$("#txtHeader").append(labels.txtHeader);
	$("#frmRealName").append(labels.frmRealName);
	$("#frmUsername").append(labels.frmUsername);
	$("#frmPassword").append(labels.frmPassword);
	$("#frmPasswordRepeat").append(labels.frmPasswordRepeat);
	$("#btnRegister").append(labels.btnRegister);
	$("#btnGoBack").append(labels.btnGoBack);
	// Set text variables for possible later use
	lblRegistrationFailed = labels.lblRegistrationFailed;
	lblUsernameFail = labels.lblUsernameFail;
	lblPasswordsMatchFail = labels.lblPasswordsMatchFail;
	lblUserTaken = labels.lblUserTaken;
	lblServerError = labels.lblServerError
}

function registerEvent() {
	$("#btnRegister").click(function() {
		register();
	});
	$("#btnGoBack").click(function() {
		document.location.href="intro.html";
	});
	// Register on pressing enter
	$("#regPasswordRepeat").keypress(function(event) {
		if (event.which == 13) {
			register();
		}
	});
}

function setRegisterFailureText(text) {
	alertWell = $.el.div({'class':'registerMessage'},
			$.el.h5({'class':'text-danger'}, text));
	// clear the current
	$("#messageWell").empty();
	$("#messageWell").append(alertWell);
}

function register() {
	// Get and check initial form input
	var name = $("#regRealName").val();
	var user = $("#regUsername").val();
	var userUri = getUserUriBase() + user;
	var password = $("#regPassword").val();
	var passwordRepeat = $("#regPasswordRepeat").val();

	if((name == "") || (user == "") || (password == "") || (passwordRepeat == "")){
		setRegisterFailureText(lblRegistrationFailed);
	} else if (checkUsername(user)) {
		setRegisterFailureText(lblUsernameFail);
	} else if (password != passwordRepeat){
		setRegisterFailureText(lblPasswordsMatchFail);
	} else {
		// Attempt registration
		registerServer(name, userUri, password);
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
	    		setRegisterFailureText(lblUserTaken);
	        } else {
	        	setRegisterFailureText(lblServerError);
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
					nextPage()();
				});
		   }
	});
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
