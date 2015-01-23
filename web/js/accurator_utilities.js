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

//Server info
server = {
location: getServerUrl()
}

function getServerUrl() {
	var urlParts = document.location.href.split("register");
	return urlParts[0];
}

function getUserUriBase() {
	return 'http://semanticweb.cs.vu.nl/user/';
}