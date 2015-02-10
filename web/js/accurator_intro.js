/* Accurator Intro
*/
var locale, ui, domain;

function introInit() {
	locale = getLocale();
	domain = getParameterByName("domain");
	ui = getUiUri(domain, "intro");
	setBackground();
	
	// If user is logged in go to profile page otherwise show intro.
	onSuccess = function() {
		document.location.href="profile.html";
	};
	onFail = function() {
		populateUI(ui);
	};
	userLoggedIn(onSuccess, onFail);
}

function setBackground() {
	// Set a background according to the specified domain
	backgroundUrl = "img/background/" + domain + ".jpg";
	backgroundDarkUrl = "img/background/" + domain + "-dark.jpg";
	backgroundGeneric = "img/background/generic.jpg";
	
	if(domain === "") {
		$(".backgroundImage").attr("src", backgroundGeneric);
		return;
	}
	
	$.ajax({url:backgroundUrl,
		    type:'HEAD',
		    success: function() {
				$(".backgroundImage").attr("src", backgroundUrl);
		    },
		    error: function() {
		        $.ajax({url:backgroundDarkUrl,
					   type:'HEAD',
					   success: function() {
						   $(".backgroundImage").attr("src", backgroundDarkUrl);
						   lightFontColor();
					   },
					   error: function() {
					       $(".backgroundImage").attr("src", backgroundGeneric);
					   }
				});
		    }
	});
}

function lightFontColor() {
	$("#txtSlogan").css('color', '#FFFFFF');
	$("#btnLogin").css('color', '#BBBBBB');
}

function populateUI(uiLocal) {
	$.getJSON("ui_elements", {locale:locale, ui:uiLocal, type:"labels"})
		.done(function(data){
			  addButtonEvents();
			  initLabels(data);})
		.fail(function(data, textStatus){
			  //Use generic ui elements
			  populateUI(getGenericUiUri("intro"));
	});
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