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
		var onDismissal = function() {document.location.href="intro.html"};
		login(drawPage, onDismissal);
	});

	function drawPage(userData) {
		var user = userData.user;
		var userName = getUserName(user);
		var realName = userData.real_name;

		setLinkLogo("profile");
		populateNavbar(userName, [], locale);
		populateRecentlyAnnotated(user);

		domainSettings(domain)
		.then(function(domainData) {
			return getLabels(locale, domainData.ui + "profile");
		})
		.then(function(labels) {
			addButtonEvents(user);
			initLabels(labels);
			initDomains(locale, domain, labels);
		});
	}
}

function populateRecentlyAnnotated(user) {
	$.getJSON("annotations", {uri:user, type:"user"})
	.then(function(uris){
		if (uris.length === 0) {
			$("#profileDivLastAnnotated").hide();
		} else {
			//TODO: limit length of uris (faster if someone annotated a bunch)?
			var cluster = new Cluster(uris, "profileDivCluster");

			$("#profileDivLastCluster").append(cluster.node);

			cluster.enrich()
			.then(function() {
				// display 6 elements in cluster
				cluster.display(6);
			});
		}
	});
}

function initLabels(labels) {
	// add retrieved labels to html elements
	document.title = labels.profilePageTitle;
	// check if real name is available
	if (typeof realName !== 'undefined') {
		$("#profileHdrSlogan").prepend(labels.profileHdrSlogan + " " + realName + " ");
	} else {
		$("#profileHdrSlogan").prepend(labels.profileHdrSlogan);
	}
	$("#profileTxtSubSlogan").prepend(labels.profileTxtSubSlogan);
	$("#profileTxtStartAnnotating").append(labels.profileTxtStartAnnotating);
	$("#navbarBtnRecommend").append(labels.navbarBtnRecommend);
	$("#profileBtnChangeExpertise").append(labels.profileBtnChangeExpertise);
	$("#navbarBtnSearch").append(labels.navbarBtnSearch);
	$("#profileBtnDomain").prepend(labels.profileBtnDomain);
	$("#profileLblLastAnnotated").append(labels.profileLblLastAnnotated);
}

function initDomains(locale, domain, labels) {
	getAvailableDomains()
	.then(function(domains) {
		// set domain settings for all the domains
		for(var i=0; i<domains.length; i++) {
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
	.then(function(data){
		$("#profileLstDomainItems").append(
			$.el.li(
				$.el.a({'href':'#',
						'id':domainData.domain},
						 data.domainLabel)));
		addDomainEvent(domainData.domain);
	});
}

function addDomainEvent(domain) {
	// add event reloadingn page on domain selection, saving choice
	$("#" + domain).click(function() {
		setDomain(domain)
		.then(function() {
			location.reload();
		});
	});
}

function addButtonEvents(user) {
	$("#navbarBtnRecommend").click(function() {
		document.location.href="results.html" + "?user=" + user;
	});
	// Search on pressing enter
	$("#navbarInpSearch").keypress(function(event) {
		if (event.which == 13) {
			var query = encodeURIComponent($("#navbarInpSearch").val());
			document.location.href="results.html?query=" + query;
		}
	});
	$("#navbarBtnSearch").click(function() {
		var query = encodeURIComponent($("#navbarInpSearch").val());
		document.location.href="results.html?query=" + query;
	});
	$("#profileBtnChangeExpertise").click(function() {
		document.location.href="expertise.html";
	});
}
