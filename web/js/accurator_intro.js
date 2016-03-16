/*******************************************************************************
Accurator Intro
Code for showing the welcom page, adapts to the domain and locale setting.
*******************************************************************************/
"use strict";

function introInit() {
	// Get settings
	var locale = getLocale();
	var domain = getDomain();

	// Add language switch to navbar
	populateFlags(locale);

	userLoggedIn()
	.then(function() {
		// Go to profile page if logged in
		document.location.href="profile.html";
	}, function() {
		// Get domain settings
		return domainSettings(domain)
	})
	.then(function(domainSettings) {
		var ui = getUI(domainSettings, "intro");
		setBackground(domainSettings.image, domainSettings.image_brightness);
		return getLabels(locale, ui);
	})
	.then(function(labels) {
		addButtonEvents();
		initLabels(labels);
	});
}

function setBackground(backgroundUrl, imageBrightness) {
	$(".introImgBackground").attr("src", backgroundUrl);

	if (imageBrightness === "dark") {
	   // Make font lighter to make it readable
	   $("#introHdrSlogan").css('color', '#FFFFFF');
	   $("#introBtnLogin").css('color', '#BBBBBB');
	}
}

function addButtonEvents() {
	$("#introBtnRegister").click(function() {
		var onDismissal = function() {
			$("#registerDivRegister").modal('hide');
		};
		registerModal(onDismissal);
	});
	$("#introBtnLogin").click(function() {
		// Show login modal and on success go to profile
		var onSuccess = function() {
			document.location.href="profile.html";
		};
		var onDismissal = function() {
			$("#loginDivLogin").modal('hide');
		};
		loginModal(onSuccess, onDismissal);
	});
}

function initLabels(labels) {
	// Add retrieved labels to html elements
	$("#introHdrSlogan").prepend(labels.introHdrSlogan);
	$("#introHdrSubSlogan").prepend(labels.introHdrSubSlogan);
	$("#introBtnRegister").append(labels.introBtnRegister);
	$("#introBtnLogin").append(labels.introBtnLogin);
	$("#introLnkAbout").append(labels.introLnkAbout);
}
