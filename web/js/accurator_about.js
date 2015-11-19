/* Accurator About
*/
var locale, domain;

function aboutInit() {
	locale = getLocale();
	domain = getDomain();

	populateFlags(locale);

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
	$("#aboutHdrAccuratorSlogan").append(labels.aboutHdrAccuratorSlogan);
	$("#aboutTxtAccurator").append(labels.aboutTxtAccurator);
	$("#aboutHdrSealincMediaSlogan").append(labels.aboutHdrSealincMediaSlogan);
	$("#aboutTxtSealincMedia").append(labels.aboutTxtSealincMedia);
	$("#aboutHdrSealincResearchSlogan").append(labels.aboutHdrSealincResearchSlogan);
	$("#aboutTxtSealincResearch").append(labels.aboutTxtSealincResearch);
	$("#aboutBtnGoBackHome").append(labels.aboutBtnGoBackHome);
}

function addButtonEvents() {
	$("#aboutBtnGoBackHome").click(function() {
		document.location.href="intro.html";
	});
}
