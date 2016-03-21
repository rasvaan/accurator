/*******************************************************************************
Accurator About
Code for showing the about page.
*******************************************************************************/
"use strict";

function aboutInit() {
	var locale = getLocale();
	var domain = getDomain();

	populateFlags(locale);

	userLoggedIn()
	.then(function(userData) {
		setLinkLogo("profile");
		var userName = getUserName(userData.user);
		populateNavbar(userName, [{link:"profile.html", name:"Profile"}], locale);
		return domainSettings(domain);
	}, function() {
		return domainSettings(domain);
	})
	.then(function(domainSettings) {
		var ui = getUI(domainSettings, "about");
		return getLabels(locale, ui);
	})
	.then(function(labels) {
		initLabels(labels);
		addButtonEvents();
	});
}

function initLabels(labels) {
	document.title = labels.aboutPageTitle;
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
