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
		var onDismissal = function() {
			document.location.href = "intro.html";
		};

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

function populateDomains(locale, domainLabels) {
	var row;

	// remove generic from the domains (does not work on ie 7 and 8..)
	domainLabels.splice(domainLabels.indexOf("generic"), 1);

	// get domain settings for all the domains
	for (var i = 0; i < domainLabels.length; i++) {
		// add a new row for every two domains
		if (i%2 === 0) {
			row = parseInt(i/2);
			$(".domainDiv").append(
				$.el.div({'class':'row',
						  'id':'domainDiv' + row})
			);
		}

		// define function to keep stable track of row
		var addDomain = function (row, locale) {
			return function(domainData) {
				domainHtml(domainData, row, locale);
			}
		}

		$.getJSON("domains", {domain:domainLabels[i]})
		.then(addDomain(row, locale));
	}
}

function domainHtml(domainData, row, locale) {
	var domain = domainData.domain;

	getLabels(locale, domainData.hasUI + "domain")
	.then(function(labels) {
		$("#domainDiv" + row).append(
			$.el.div({'class':'noPadding col-md-6'},
				$.el.h3({'class':'domainHdr',
						 'id':'domainTxt' + domain},
						 labels.domainLabel),
				$.el.img({'class':'domainImg',
						  'id':'domainImg' + domain,
						  'src':domainData.image})));

		if(domainData.imageBrightness === "dark")
			$("#domainTxt" + domainData.domain).css('color', '#fff');

		addDomainEvent(domain);
	});
}

function addDomainEvent(domain) {
	$("#domainImg" + domain).click(function() {
		setDomain(domain)
		.then(function() {
			document.location.href = "results.html";
		});
	});
	$("#domainTxt" + domain).click(function() {
		setDomain(domain)
		.then(function() {
			document.location.href = "results.html";
		});
	});
}
