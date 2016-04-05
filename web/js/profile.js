/*******************************************************************************
Accurator Profile
Code for showing statistical elements on the profile page and allowing the user
to change settings.
*******************************************************************************/
"use strict";

function profileInit() {
	var locale = getLocale();
	var domain = getDomain();

	populateFlags(locale);

	userLoggedIn()
	.then(function(userData) {
		// user is logged in, so draw page
		drawPage(userData);
	}, function() {
		// user is not logged in, show modal
		var onDismissal = function() {
			document.location.href = "intro.html";
		};

		login(drawPage, onDismissal);
	});

	function drawPage(userData) {
		var user = userData.user;
		var userName = getUserName(user);
		var realName = userData.real_name;

		setLinkLogo("profile");
		populateNavbar(userName, [], locale);


		domainSettings(domain)
		.then(function(domainData) {
			return getLabels(locale, domainData.ui + "profile");
		})
		.then(function(labelData) {
			addButtonEvents(user);
			var labels = initLabels(labelData, realName);
			initDomains(locale, domain, labels);
			populateRecentlyAnnotated(user, labels);
		});
	}
}

function populateRecentlyAnnotated(user, labels) {
	$.getJSON("annotations", {
		uri:user,
		type:"user"
	})
	.then(function(uris){
		if (uris.length === 0) {
			$("#profileDivLastAnnotated").hide();
		} else {
			//TODO: limit length of uris (faster if someone annotated a bunch)?
			var noRecentlyAnnotatedItems = 6;

			var cluster = new Cluster(
				"profileDivCluster",
				uris,
				labels.profileLblLastAnnotated
			);

			cluster.init(noRecentlyAnnotatedItems)
			.then(function() {
				$("#profileDivLastAnnotated").append(cluster.node);
			});
		}
	});
}

function initLabels(labelData, realName) {
	// add retrieved labels to html elements
	document.title = labelData.profilePageTitle;

	var labels = {
		profileLblLastAnnotated: labelData.profileLblLastAnnotated,
		profileTxtDomain: labelData.profileTxtDomain
	};

	// check if real name is available
	if (typeof realName !== 'undefined') {
		$("#profileHdrSlogan").prepend(labelData.profileHdrSlogan + " " + realName + " ");
	} else {
		$("#profileHdrSlogan").prepend(labelData.profileHdrSlogan);
	}
	$("#profileTxtSubSlogan").prepend(labelData.profileTxtSubSlogan);
	$("#profileTxtStartAnnotating").append(labelData.profileTxtStartAnnotating);
	$("#navbarBtnRecommend").append(labelData.navbarBtnRecommend);
	$("#profileBtnChangeExpertise").append(labelData.profileBtnChangeExpertise);
	$("#navbarBtnSearch").append(labelData.navbarBtnSearch);
	$("#profileBtnDomain").prepend(labelData.profileBtnDomain);

	return labels;
}

function initDomains(locale, domain, labels) {
	getAvailableDomains()
	.then(function(domains) {
		// set domain settings for all the domains
		for(var i = 0; i < domains.length; i++) {
			var currentDomain = domains[i];

			// already create function so currentdomain is not the last deu to asynchronisity
			var processDomain = function(currentDomain, labels) {
				return function(domainData) {
					if (domain === currentDomain) {
						addDomainTitle(domainData, locale, labels);
					} else {
						domainHtml(domainData, locale);
					}
				}
			}

			// add info about all domains except generic
			if(currentDomain !== "generic") {
				$.getJSON("domains", {domain:currentDomain})
				.then(processDomain(currentDomain, labels));
			}
		}

		// hide button if length is 2 (current domain and generic)
		if (domain !== "generic" && domains.length === 2) {
			$("#profileBtnDomain").parent().hide();
		}
	});
}

function addDomainTitle(domainData, locale, labels) {
	// add the title of the current domain to the profile page
	getLabels(locale, domainData.ui + "domain")
	.then(function(data){
		$("#profileTxtDomain").append(
			labels.profileTxtDomain,
			$.el.span({'class':'text-info'},
						data.domainLabel));}
	);
}

function domainHtml(domainData, locale) {
	// add the different domains to a dropdown list
	getLabels(locale, domainData.ui + "domain")
	.then(function(data) {
		$("#profileLstDomainItems").append(
			$.el.li(
				$.el.a({'href':'#',
						'id':domainData.domain},
						 data.domainLabel)));

		addDomainEvent(domainData.domain);
	});
}

function addDomainEvent(domain) {
	// add event reloading page on domain selection, saving choice
	$("#" + domain).click(function() {
		setDomain(domain)
		.then(function() {
			location.reload();
		});
	});
}

function addButtonEvents(user) {
	$("#navbarBtnRecommend").click(function() {
		document.location.href = "results.html" + "?user=" + user;
	});
	// Search on pressing enter
	$("#navbarInpSearch").keypress(function(event) {
		if (event.which == 13) {
			var query = encodeURIComponent($("#navbarInpSearch").val());

			document.location.href = "results.html?query=" + query;
		}
	});
	$("#navbarBtnSearch").click(function() {
		var query = encodeURIComponent($("#navbarInpSearch").val());

		document.location.href = "results.html?query=" + query;
	});
	$("#profileBtnChangeExpertise").click(function() {
		document.location.href = "expertise.html";
	});
}
