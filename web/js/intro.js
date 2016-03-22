/*******************************************************************************
Accurator Intro

Code for showing the welcome page, adapts to the domain and locale setting.
*******************************************************************************/
"use strict";

function introInit() {
	var locale = getLocale();
	var domain = getDomain();

	// add language switch to navbar
	populateFlags(locale);

	userLoggedIn()
	.then(function() {
		// go to profile page if logged in
		document.location.href = "profile.html";
	}, function() {
		// get domain settings
		return domainSettings(domain);
	})
	.then(function(domainSettings) {
		var ui = getUI(domainSettings, "intro");
		setBackground(domainSettings.image, domainSettings.image_brightness);
		return getLabels(locale, ui);
	})
	.then(function(labels) {
		addButtonEvents({"locale": locale, "domain": domain});
		initLabels(labels);
	});
}

function setBackground(backgroundUrl, imageBrightness) {
	$(".introImgBackground").attr("src", backgroundUrl);

	if (imageBrightness === "dark") {
	   // make font lighter to make it readable
	   $("#introHdrSlogan").css('color', '#FFFFFF');
	   $("#introBtnLogin").css('color', '#BBBBBB');
	}
}

function addButtonEvents(settings) {
	$("#introBtnRegister").click(function() {
		var onDismissal = function() {
			$("#registerDivRegister").modal('hide');
		};
		
		registerModal(onDismissal, settings);
	});
	$("#introBtnLogin").click(function() {
		// show login modal and on success go to profile
		var onSuccess = function() {
			document.location.href = "profile.html";
		};
		var onDismissal = function() {
			$("#loginDivLogin").modal('hide');
		};

		login(onSuccess, onDismissal);
	});
}

function initLabels(labels) {
	// add retrieved labels to html elements
	$("#introHdrSlogan").prepend(labels.introHdrSlogan);
	$("#introHdrSubSlogan").prepend(labels.introHdrSubSlogan);
	$("#introBtnRegister").append(labels.introBtnRegister);
	$("#introBtnLogin").append(labels.introBtnLogin);
	$("#introLnkAbout").append(labels.introLnkAbout);
}
