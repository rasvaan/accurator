/* Accurator Register
*/
var locale;
var ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#register";
var lblRegistrationFailed, lblPasswordsMatchFail, lblUserTaken, lblServerError;

function registerInit() {
	locale = getLocale();
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
		.done(function(data){
			  registerEvent();
			  initLabels(data);
			  $("#frmRealName").focus();})
		.fail(function(data, textStatus){
			  setRegisterFailureText("Problem connecting to server, please contact the system administrator.");});
}

function initLabels(data) {
	$("#txtHeader").append(data.txtHeader);
	$("#frmRealName").append(data.frmRealName);
	$("#frmUsername").append(data.frmUsername);
	$("#frmPassword").append(data.frmPassword);
	$("#frmPasswordRepeat").append(data.frmPasswordRepeat);
	$("#btnRegister").append(data.btnRegister);
	lblRegistrationFailed = data.lblRegistrationFailed;
	lblPasswordsMatchFail = data.lblPasswordsMatchFail;
	lblUserTaken = data.lblUserTaken;
	lblServerError = data.lblServerError
}

function registerEvent() {
	$("#btnRegister").click(function() {
		register();
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
	var name = $("#regRealName").val();
	var user = getUserUriBase() + $("#regUsername").val();
	var password = $("#regPassword").val();
	var passwordRepeat = $("#regPasswordRepeat").val();
	
	if((name == "") || (user == "") || (password == "") || (passwordRepeat == "")){
		setRegisterFailureText(lblRegistrationFailed);
	} else if (password != passwordRepeat){
		setRegisterFailureText(lblPasswordsMatchFail);
	} else {
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
		   loginServer(user, password, function(){document.location.href="additional_info.html";})
		},
		error: function (request, textStatus, errorThrown) {
			if(errorThrown == "Not Found")
	        	setRegisterFailureText("Server did not respond.");
	        if(request.responseText.contains("User already exists")) {
	    		setRegisterFailureText(lblUserTaken);
	        } else {
	        	setRegisterFailureText(lblServerError);
	        }
		}
	});
}