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

		$.getJSON("domains", {domain:domainLabels[i]})
		.then(addDomain(row, locale));
	}
}

function addDomain(row, locale) {
	return function(domainData) {
		return getLabels(locale, domainData.hasUI + "domain")
		.then(function(labels) {
			var topics = null;
			var subDomains;

			// see if there are subdomains
			domainData.subDomains ?
				subDomains = domainData.subDomains
				: subDomains = [];

			// see if there is info about expertise topics
			if (domainData.requires) {
				topics = new ExpertiseTopics (
					domainData.requires,
					domainData.hasTopConcept,
					domainData.hasMaximumExpertiseTopics,
					domainData.hasMaximumChildren
				);
			}

			// domainData
			var domain = new Domain (
				domainData.domain,
				labels.domainLabel,
				domainData.image,
				domainData.imageBrightness,
				subDomains,
				topics
			);

			$("#domainDiv" + row).append(domain.node);
		});
	}
}
