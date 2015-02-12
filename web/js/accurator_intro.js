/* Accurator Intro
*/
var locale, ui, domain, domainSettings;

function introInit() {
	locale = getLocale();
	domain = getParameterByName("domain");
	
	// If user is logged in go to profile page
	onSuccess = function() {
		document.location.href="profile.html";
	};
	// If user is not logged in populate intro page
	onFail = function() {
		//Get domain settings before populating ui
		var onDomain = function(data) {
			setBackground(data.image, data.image_brightness);
			populateUI(data.ui + "intro");
		}
		domainSettings = domainSettings(domain, onDomain);
	};
	userLoggedIn(onSuccess, onFail);
}

function setBackground(backgroundUrl, imageBrightness) {
	if(imageBrightness=== "light") {
		$(".backgroundImage").attr("src", backgroundUrl);
	} else if (imageBrightness === "dark") {
	   $(".backgroundImage").attr("src", backgroundDarkUrl);
	   // Make font lighter
	   $("#txtSlogan").css('color', '#FFFFFF');
	   $("#btnLogin").css('color', '#BBBBBB');
	}
}

function populateUI(uiLocal) {
	alert(uiLocal);
	$.getJSON("ui_elements", {locale:locale, ui:uiLocal, type:"labels"})
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