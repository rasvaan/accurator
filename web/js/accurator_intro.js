/* Accurator Intro
*/
var locale, domain, experiment, ui, domainSettings;

function introInit() {
	locale = getLocale();
	domain = getDomain();
	experiment = getExperiment();

	populateFlags(locale);

	// If user is logged in go to profile page
	onLoggedIn = function() {
		document.location.href="profile.html";
	};
	// If user is not logged in populate intro page
	onNotLoggedIn = function() {
		//Get domain settings before populating ui
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
	$("#btnRegister").click(function() {
		document.location.href="register.html";
	});
	$("#btnLogin").click(function() {
		//Show login modal and on success go to profile
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
	$("#txtSlogan").prepend(labels.txtSlogan);
	$("#txtSubSlogan").prepend(labels.txtSubSlogan);
	$("#btnRegister").append(labels.btnRegister);
	$("#btnLogin").append(labels.btnLogin);
	$("#lnkAbout").append(labels.lnkAbout);
}
