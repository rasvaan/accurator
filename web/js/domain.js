/*******************************************************************************
Accurator Domain

This code loads domain options into the page, where the options depend on the
domains loaded in the triple store.
*******************************************************************************/
function domainInit() {
	var locale = getLocale();
	var domain = getParameterByName("domain");
	if (domain === "") domain = "generic"; // be domain agnostic

	// add language switch to navbar
	populateFlags(locale);

	userLoggedIn()
	.then(function(userData) {
		drawPage(userData);
	}, function() {
		var onDismissal = function() {document.location.href = "intro.html";};

		login(drawPage, onDismissal);
	});

	function drawPage(userData) {
		var labels;
		var userName = getUserName(userData.user);
		var ui = "http://accurator.nl/ui/generic#domain"; // get generic settings for labels

		setLinkLogo("profile");
		populateNavbar(userName, [{link:"profile.html", name:"Profile"}], locale);

		getLabels(locale, ui)
		.then(function(labelData) {
			labels = initLabels(labelData, domain)
			return getDomains(domain);
		})
		.then(function(domains) {
			populateDomains(domains, domain, labels, locale); // draw all domains
		});
	}
}

function initLabels(labels, domain) {
	document.title = labels.domainPageTitle;

	if (domain === "generic") {
		// show initial text
		$("#domainTxtTitle").append(labels.domainHdr);
	} else {
		// show text for subdomains
		$("#domainTxtTitle").append(labels.domainHdrSub);
	}

	return {'domainTxtAllObjects':labels.domainTxtAllObjects};
}

function getDomains(domain) {
	// provide a promise that either gives all root domains or the subdomains
	if (domain === "generic") {
		return getAvailableDomains();
	} else {
		return domainSettings(domain).then(function(data) {
			var domains = [];

			// process the retrieved subdomain uris to get domain label
			for (var i=0; i<data.subDomains.length; i++) {
				domains[i] = generateDomainFromUri(data.subDomains[i]);
			}

			return domains;
		});
	}
}

function populateDomains(domains, domain, labels, locale) {
	var row;

	// remove generic from the domains if showing top domains
	if (domain === "generic") {
		// only remove when there are other domains available
		if (domains.length > 1)
			domains.splice(domains.indexOf("generic"), 1);
	} else {
		// add root domain as option to select in list of subdomains
		domains.push(domain);
	}

	// get domain settings for all the domains
	for (var i=0; i<domains.length; i++) {
		// add a new row for every two domains
		if (i%2 === 0) {
			row = parseInt(i/2);
			$(".domainDiv").append(
				$.el.div({'class':'row',
						  'id':'domainDiv' + row})
			);
		}

		$.getJSON("domains", {domain:domains[i]})
		.then(addDomain(row, locale, labels, domain));
	}
}

function addDomain(row, locale, pageLabels, rootDomain) {
	return function(domainData) {
		return getLabels(locale, domainData.hasLabel)
		.then(function(domainLabel) {
			var topics = null;
			var subDomains, domainLabel;
			var link = "results.html"; // go to results page by default

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

			if (rootDomain === domainData.domain) {
				// set domain label for root domain if matched
				domainLabel = pageLabels.domainTxtAllObjects + domainLabel.textLabel;
			} else {
				// set domain label and link for subdomain
				domainLabel = domainLabel.textLabel;

				if (subDomains.length > 0) {
					link = "domain.html?domain=" + domainData.domain;
				} else if (topics != null) {
					link = "expertise.html";
				}
			}

			// create domain object
			var domain = new Domain (
				domainData.domain,
				domainLabel,
				link,
				domainData.image,
				domainData.imageBrightness,
				subDomains,
				topics
			);

			$("#domainDiv" + row).append(domain.node);
		});
	}
}
