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
	var row;
	console.log("got the following topics ", domainSettings.sub_domains);

	// get topic settings for all the topics
	for(var i = 0; i < domainSettings.sub_domains.length; i++) {
		if(!(i%2 === 0)) {
			row = parseInt((i/2) + 0.5);
			// Add a new row for every two domains
			$(".topicDiv").append(
				$.el.div({'class':'row',
						  'id':'topic' + row}));
		}

		// add domain specific html to rows
		$.getJSON("domains", {domain:domainSettings.sub_domains[i]})
		.then(function(topicData) {
			topicHtml(topicData, row, locale);
		});
	}
}

function topicHtml(topicData, row, locale) {
	console.log("Adding topic for ", topicData);
// var domain = domainData.domain;
//
// 	getLabels(locale, domainData.ui + "domain")
// 	.then(function(labels) {
// 		$("#domain" + row).append(
// 			$.el.div({'class':'noPadding col-md-6'},
// 				$.el.h3({'class':'domainHdr',
// 						 'id':'domainTxt' + domain},
// 						 labels.domainLabel),
// 				$.el.img({'class':'domainImg',
// 						  'id':'domainImg' + domain,
// 						  'src':domainData.image})));
//
// 		if(domainData.image_brightness === "dark")
// 			$("#domainTxt" + domainData.domain).css('color', '#fff');
//
// 		addDomainEvent(domain);
// 	});
}
//
// function addDomainEvent(domain) {
// 	$("#domainImg" + domain).click(function() {
// 		setDomain(domain)
// 		.then(function() {
// 			document.location.href = "results.html";
// 		});
// 	});
// 	$("#domainTxt" + domain).click(function() {
// 		setDomain(domain)
// 		.then(function() {
// 			document.location.href = "results.html";
// 		});
// 	});
// }
