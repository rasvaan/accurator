/* Accurator About
*/
var locale, domain;

function aboutInit() {
	locale = getLocale();
	domain = getDomain();
	
	var onDomain = function(domainData) {
		populateUI(domainData);
	}
	var onLoggedIn = function(data){
		setLinkLogo("profile");
		userName = getUserName(data.user);
		populateNavbar(userName, [{link:"profile.html", name:"Profile"}]);
		domainSettings = domainSettings(domain, onDomain);
	};
	var onNotLoggedIn = function(){
		domainSettings = domainSettings(domain, onDomain);
	};
	userLoggedIn(onLoggedIn, onNotLoggedIn);
}

function populateUI(data) {
	var ui = data.ui + "about"

	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
		.done(function(data){
			initLabels(data);});
}

function initLabels(data) {
	$("#txtAboutSlogan").append(data.txtAboutSlogan);
	$("#txtAboutAccurator").append(data.txtAboutAccurator);
	$("#txtSealincMediaSlogan").append(data.txtSealincMediaSlogan);
	$("#txtAboutSealincMedia").append(data.txtAboutSealincMedia);
	$("#txtSealincResearchSlogan").append(data.txtSealincResearchSlogan);
	$("#txtAboutSealincResearch").append(data.txtAboutSealincResearch);
}