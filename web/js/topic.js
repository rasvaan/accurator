/*******************************************************************************
Accurator Topic

This code loads topic options into the page, where the options depend on the
topics included in a domain the triple store.
*******************************************************************************/
function topicInit() {
	var locale = getLocale();
	var domain = getDomain();

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
		setLinkLogo("profile");

		domainSettings(domain)
		.then(function(domainSettings) {
			var ui = getUI(domainSettings, "topic");

			// draw all topics
			populateTopics(locale, domainSettings);
			return getLabels(locale, ui);
		})
		.then(function(labels) {
			document.title = labels.topicPageTitle;
			$("#topicTxtTitle").append(labels.topicTxtTitle);
			var userName = getUserName(userData.user);
			populateNavbar(userName, [{link:"profile.html", name:"Profile"}], locale);
		});
	}
}

function populateTopics(locale, domainSettings) {
	// var row;
	console.log("got the following topics ", domainSettings.subDomains);

	// // get topic settings for all the topics
	// for(var i = 0; i < domainSettings.sub_domains.length; i++) {
	// 	if(!(i%2 === 0)) {
	// 		row = parseInt((i/2) + 0.5);
	// 		// Add a new row for every two domains
	// 		$(".topicDiv").append(
	// 			$.el.div({'class':'row',
	// 					  'id':'topic' + row}));
	// 	}
	//
	// 	// add domain specific html to rows
	// 	$.getJSON("domains", {domain:domainSettings.sub_domains[i]})
	// 	.then(function(topicData) {
	// 		topicHtml(topicData, row, locale);
	// 	});
	// }
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
