/* Accurator Register
*/
var locale, domain, ui;
var lblRegistrationFailed, lblPasswordsMatchFail, lblUserTaken, lblServerError;

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
	var user = getUserUriBase() + $("#regUsername").val();
	var password = $("#regPassword").val();
	var passwordRepeat = $("#regPasswordRepeat").val();
	
	if((name == "") || (user == "") || (password == "") || (passwordRepeat == "")){
		setRegisterFailureText(lblRegistrationFailed);
	} else if (password != passwordRepeat){
		setRegisterFailureText(lblPasswordsMatchFail);
	} else {
		// Attempt registration
		registerServer(name, user, password);
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
			// Clear domain and locale
			clearLocalStorage("locale");
			clearLocalStorage("domain");
			// Login new user
		   loginServer(user, password, function(){document.location.href="additional_info.html";})
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