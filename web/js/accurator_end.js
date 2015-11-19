/*******************************************************************************
Accurator End
Code for showing the last page, adapts to the domain and locale setting.
*******************************************************************************/
var locale, domain, experiment, ui, domainSettings;

function endInit() {
	// Get settings
	locale = getLocale();
	domain = getDomain();
	experiment = getExperiment();

	// Add language switch to navbar
	populateFlags(locale);

	// Get domain settings before populating ui
	onDomain = function(domainSettings) {
		ui = getUI(domainSettings, "end");
		setBackground(domainSettings.last_image,
					  domainSettings.last_image_brightness);
		populateUI();
	};
	domainSettings = domainSettings(domain, onDomain);
	// If user is logged in go to profile page
	onLoggedIn = function() {

	};
	// If user is not logged in populate end page
	onNotLoggedIn = function() {};
	userLoggedIn(onLoggedIn, onNotLoggedIn);
}

function setBackground(backgroundUrl, imageBrightness) {
	$(".backgroundImage").attr("src", backgroundUrl);

	if (imageBrightness === "dark") {
	   // Make font lighter to make it readable
	   $("#txtSlogan").css('color', '#FFFFFF');
	   $("#btnLogin").css('color', '#BBBBBB');
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
	$("#btnContinue").click(function() {
		window.location.href="http://annotate.accurator.nl";
	});
}

function initLabels(labels) {
	// Add retrieved labels to html elements
	$("#txtSlogan").prepend(labels.txtSlogan);
	$("#txtSubSlogan").prepend(labels.txtSubSlogan);
	$("#btnContinue").append(labels.btnContinue);
	$("#navbarLnkAbout").append(labels.navbarLnkAbout);
}
