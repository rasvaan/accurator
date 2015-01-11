/* Accurator Intro
*/

function introInit() {
	slogan = "Accurate Art Annotations";
	subSlogan = "Help us add information to the	artworks depicting birds";
	register = "Register";
	login = "or Log in";
	mdlTitle = "Log in";
	mdlBtnLogin = "Log in";
	mdlUsername = "Username";
	mdlPassword = "Password";
	registerButtonEvent();
	addTextSlogan(slogan, subSlogan);
	addTextButton(register, login);
	addTextModal(mdlTitle, mdlBtnLogin, mdlUsername, mdlPassword);
}

function addTextSlogan(slogan, subSlogan) {
	$("#txtSlogan").prepend(slogan);
	$("#txtSubSlogan").prepend(subSlogan);
}

function addTextButton(register, login) {
	$("#btnRegister").append(register);
	$("#btnLogin").append(login);
}

function addTextModal(mdlTitle, mdlBtnLogin, mdlUsername, mdlPassword) {
	$("#mdlTitle").append(mdlTitle);
	$("#mdlBtnLogin").append(mdlBtnLogin);
	$("#mdlUsername").append(mdlUsername);
	$("#mdlPassword").append(mdlPassword);
}

function registerButtonEvent() {
	$("#btnRegister").click(function() {
		document.location.href="/register.html";
	});	
}