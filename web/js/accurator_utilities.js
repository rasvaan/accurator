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
	.fail(function(){alert("No user logged in.");});
}

function getUserUriBase() {
	return 'http://semanticweb.cs.vu.nl/user/';
}