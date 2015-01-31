/* Accurator Utilities
*/

//Locale
function getLocale() {
	if(localStorage.getItem("locale") === null){
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

function setLocale(languageCode) {
	localStorage.setItem("locale", languageCode);
}


//User
function userLoggedIn(initFunction) {
	//get the user id
	$.getJSON("get_user")
	.done(initFunction)
	.fail(function(){loginModal()});
}

function loginModal() {
	$.getJSON("ui_elements", {locale:getLocale(), ui:"http://semanticweb.cs.vu.nl/accurator/ui/bird#login_modal", type:"labels"})
	.done(function(data){
		  $("#mdlTxtTitle").append(data.mdlTxtTitle);
		  $("#mdlBtnLogin").append(data.mdlBtnLogin);
		  $("#mdlFrmUsername").append(data.mdlFrmUsername);
		  $("#mdlFrmPassword").append(data.mdlFrmPassword);
		  $('#modalLogin').modal();});
}

function getUserUriBase() {
	return 'http://semanticweb.cs.vu.nl/user/';
}