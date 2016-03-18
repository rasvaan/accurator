/*******************************************************************************
Accurator Domain
This code loads domain options into the page, where the options depend on the
domains loaded in the triple store.
*******************************************************************************/
function domainInit() {
	var locale = getLocale();
	// be domain agnostic on domain selection screen
	var domain = "generic";

	// add language switch to navbar
	populateFlags(locale);

	userLoggedIn()
	.then(function(userData) {
		drawPage(userData);
	}, function() {
		// user is not logged in, show modal
		var onDismissal = function() {document.location.href="intro.html"};
		login(drawPage, onDismissal);
	});

	function drawPage(userData) {
		setLinkLogo("profile");

		getAvailableDomains()
		.then(function(domains) {
			// draw all domains
			populateDomains(locale, domains);
			return domainSettings(domain);
		})
		.then(function(domainSettings) {
			var ui = getUI(domainSettings, "domain");
			return getLabels(locale, ui);
		})
		.then(function(labels) {
			document.title = labels.domainPageTitle;
			$("#domainTxtTitle").append(labels.domainTxtTitle);
			var userName = getUserName(userData.user);
			populateNavbar(userName, [{link:"profile.html", name:"Profile"}], locale);
		});
	}
}

function nextPage() {
	// we do not need expertise to do random stuff
	if(experiment === "random")
		return function(){document.location.href="results.html"};

	return function(){document.location.href="expertise.html"};
}

function populateDomains(locale, domainLabels) {
	var row;

	// Get domain settings for all the domains
	for(var i=0; i<domainLabels.length; i++) {
		if(!(i%2===0)) {
			row = parseInt((i/2) + 0.5);
			// Add a new row for every two domains
			$(".domainDiv").append(
				$.el.div({'class':'row',
						  'id':'domain' + row}));
		}

		// Add domain specific html to rows
		$.getJSON("domains", {domain:domainLabels[i]})
			.done(function(data){
				if(!(data.domain === "generic")) {
					domainHtml(data, row, locale);
				}
			});
	}
}

function domainHtml(domainData, row, locale) {
	var domain = domainData.domain;
	$.getJSON("ui_elements",
			  {locale:locale,
			   ui:domainData.ui + "domain",
			   type:"labels"})
		.done(function(data){
			$("#domain" + row).append(
				$.el.div({'class':'noPadding col-md-6'},
					$.el.h3({'class':'domainHdr',
							 'id':'domainTxt' + domain},
							 data.domainLabel),
					$.el.img({'class':'domainImg',
							  'id':'domainImg' + domain,
							  'src':domainData.image})));
			if(domainData.image_brightness === "dark")
				$("#domainTxt" + domainData.domain).css('color', '#fff');
			addDomainEvent(domain);
		});
}

function addDomainEvent(domain) {
	$("#domainImg"+domain).click(function() {
		setDomain(domain, nextPage());
	});
	$("#domainTxt"+domain).click(function() {
		setDomain(domain, nextPage());
	});
}
