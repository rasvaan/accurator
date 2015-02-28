/* Accurator About
*/
var locale, domain;

function aboutInit() {
	locale = getLocale();
	domain = getDomain();
	
	// Domain settings are needed
	var onDomain = function(domainSettings) {
		populateUI(domainSettings);
	}
	var onLoggedIn = function(userData){
		setLinkLogo("profile");
		userName = getUserName(userData.user);
		populateNavbar(userName,
			[{link:"profile.html", name:"Profile"}]);
		domainSettings = domainSettings(domain, onDomain);
	};
	var onNotLoggedIn = function(){
		domainSettings = domainSettings(domain, onDomain);
	};
	userLoggedIn(onLoggedIn, onNotLoggedIn);
}

function populateUI(domainSettins) {
	ui = getUI(domainSettings, "about");

	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
		.done(function(labels){
			initLabels(labels);
			addButtonEvents();});
}

function initLabels(labels) {
	document.title = labels.title;
	$("#txtAboutSlogan").append(labels.txtAboutSlogan);
	$("#txtAboutAccurator").append(labels.txtAboutAccurator);
	$("#txtSealincMediaSlogan").append(labels.txtSealincMediaSlogan);
	$("#txtAboutSealincMedia").append(labels.txtAboutSealincMedia);
	$("#txtSealincResearchSlogan").append(labels.txtSealincResearchSlogan);
	$("#txtAboutSealincResearch").append(labels.txtAboutSealincResearch);
	$("#btnGoBackHome").append(labels.btnGoBackHome);
}

function addButtonEvents() {
	$("#btnGoBackHome").click(function() {
		document.location.href="intro.html";
	});
}