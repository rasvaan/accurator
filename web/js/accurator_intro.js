/* Accurator Intro
*/
var locale, domain, ui, domainSettings;

function introInit() {
	locale = getLocale();
	domain = getDomain();
	
	// If user is logged in go to profile page
	onLoggedIn = function() {
		document.location.href="profile.html";
	};
	// If user is not logged in populate intro page
	onNotLoggedIn = function() {
		//Get domain settings before populating ui
		onDomain = function(data) {
			ui = getUI(data, "intro");
			console.log(ui);
			setBackground(data.image, data.image_brightness);
			populateUI();
		}
		domainSettings = domainSettings(domain, onDomain);
	};
	userLoggedIn(onLoggedIn, onNotLoggedIn);
}

function setBackground(backgroundUrl, imageBrightness) {
	$(".backgroundImage").attr("src", backgroundUrl);
	
	if (imageBrightness === "dark") {
	   // Make font lighter
	   $("#txtSlogan").css('color', '#FFFFFF');
	   $("#btnLogin").css('color', '#BBBBBB');
	}
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
		.done(function(data){
			  addButtonEvents();
			  initLabels(data);});
}

function addButtonEvents() {
	$("#btnRegister").click(function() {
		document.location.href="register.html";
	});
	$("#btnLogin").click(function() {
		onSuccess = function() {
			document.location.href="profile.html";
		};
		onDismissal = function() {
			$("#modalLogin").modal('hide');
		};
		loginModal(onSuccess, onDismissal);
	});
}

function initLabels(data) {
	$("#txtSlogan").prepend(data.txtSlogan);
	$("#txtSubSlogan").prepend(data.txtSubSlogan);
	$("#btnRegister").append(data.btnRegister);
	$("#btnLogin").append(data.btnLogin);
	$("#lnkAbout").append(data.lnkAbout);
}