/* Accurator About
*/
var locale, domain;

function aboutInit() {
	locale = getLocale();
	domain = getParameterByName("domain");
	
	var onDomain = function(data) {
		populateUI(data.ui + "about");
	}
	var onLoggedIn = function(data){
		userName = getUserName(data.user);
		populateNavbar(userName, [{link:"profile.html", name:"Profile"}]);
		domainSettings = domainSettings(domain, onDomain);
	};
	var onNotLoggedIn = function(){
		domainSettings = domainSettings(domain, onDomain);
	};
	userLoggedIn(onLoggedIn, onNotLoggedIn);
}

function populateUI(ui) {
	setLinkLogo("profile");
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