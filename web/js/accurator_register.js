/* Accurator Register
*/
var locale, domain, ui;
var lblRegistrationFailed, lblUsernameFail, lblPasswordsMatchFail, lblUserTaken, lblServerError;

function registerInit() {
	locale = getLocale();
	domain = getDomain();

	var onDomain = function(domainSettings) {
		ui = getUI(domainSettings, "register");
		populateUI();
	}
	domainSettings = domainSettings(domain, onDomain);
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
	var domain = "generic";

	$.ajax({
		type: "POST",
		url: "register_user",
		contentType: "application/json",
		data: JSON.stringify(json),
		success: function(){
			// login user upon registering
			loginServer(user, password, function(){
				// reset locale since it was updated (incorrectly) by login
				localStorage.setItem("locale", locale);
				localStorage.setItem("domain", domain);
				// save current info
				save_user_info({"locale":locale,"domain":domain}, function(){
					document.location.href="additional_info.html";
				});
			});
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

function checkUsername(user) {
	var illegalChars = /\W/; // allow letters, numbers and underscores

	if (illegalChars.test(user)) {
		return true;
	} else {
		return false;
	}
}
