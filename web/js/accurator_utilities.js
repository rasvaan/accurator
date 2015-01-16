/* Accurator Utilities
*/

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