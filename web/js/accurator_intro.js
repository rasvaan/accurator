/*******************************************************************************
Accurator Intro
Code for showing the welcom page, adapts to the domain and locale setting.
*******************************************************************************/
var locale, domain, experiment, ui, domainSettings;

function introInit() {
	// Get settings
	locale = getLocale();
	domain = getDomain();
	experiment = getExperiment();

	// Add language switch to navbar
	populateFlags(locale);

	// If user is logged in go to profile page
	onLoggedIn = function() {
		document.location.href="profile.html";
	};
	// If user is not logged in populate intro page
	onNotLoggedIn = function() {
		// Get domain settings before populating ui
		onDomain = function(domainSettings) {
			ui = getUI(domainSettings, "intro");
			setBackground(domainSettings.image,
						  domainSettings.image_brightness);
			populateUI();
		};
		domainSettings = domainSettings(domain, onDomain);
	};
	userLoggedIn(onLoggedIn, onNotLoggedIn);
}

function setBackground(backgroundUrl, imageBrightness) {
	$(".introImgBackground").attr("src", backgroundUrl);

	if (imageBrightness === "dark") {
	   // Make font lighter to make it readable
	   $("#introHdrSlogan").css('color', '#FFFFFF');
	   $("#introBtnLogin").css('color', '#BBBBBB');
	}
}

function populateUI() {
	// Retrieve labels from server according to locale and ui
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
		.done(function(labels){
			  addButtonEvents();
			  initLabels(labels);});
}

function addButtonEvents() {
	$("#introBtnRegister").click(function() {
		onDismissal = function() {
			$("#modalRegister").modal('hide');
		};
		registerModal(onDismissal);
	});
	$("#introBtnLogin").click(function() {
		// Show login modal and on success go to profile
		onSuccess = function() {
			document.location.href="profile.html";
		};
		onDismissal = function() {
			$("#modalLogin").modal('hide');
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
	$("#lnkAbout").append(labels.lnkAbout);
}
