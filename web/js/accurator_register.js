/* Accurator Register
*/
var locale = "en";
var ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#register";
var lblRegistrationFailed;
var lblPasswordsMatchFail;

function registerInit() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
	.done(function(data){
		registerEvent();
		initLabels(data);})
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
	alertWell = $.el.div({'class':'well'}, 
			$.el.p({'class':'text-danger'}, text)); 
	// clear the current
	$("#messageWell").empty();
	$("#messageWell").append(alertWell);
}

function register() {
	var register = false
	var name = $("#regRealName").val();
	var user = $("#regUsername").val();
	var password = $("#regPassword").val();
	var passwordRepeat = $("#regPasswordRepeat").val();
	
	if((name == "") || (user == "") || (password == "") || (passwordRepeat == "")){
		setRegisterFailureText(lblRegistrationFailed);
	} else if (password != passwordRepeat){
		setRegisterFailureText(lblPasswordsMatchFail);
	} else {
		register = registerServer(name, user, password);
	}
	if(register)
		document.location.href="/additional_info.html";
}

function registerServer(name, user, password) {
	console.log("registering user " + name);
	//Should defenitely be loggin in to server
	return true;
}