/*******************************************************************************
Accurator Profile
Code for showing statistical elements on the profile page and allowing the user
to change settings.
*******************************************************************************/
var locale, ui, domain, experiment, user, userName, realName;
var recentItems;
var initialClusters, enrichedClusters, clusters;

display = {
	numberDisplayedItems: 6,
}

function profileInit() {
	locale = getLocale();
	domain = getDomain();
	experiment = getExperiment();

	populateFlags(locale);

	logUserIn()
	.then(function(userData) {
		console.log(userData);
	}, function(message) {
		console.log(message);
		// Go to intro page if not logged in
		// document.location.href="intro.html";
	});

	// var onLoggedIn = function(loginData) {
	// 	var deferred = jQuery.Deferred();
	//
	// 	setLinkLogo("profile");
	// 	user = loginData.user;
	// 	userName = getUserName(user);
	// 	realName = loginData.real_name;
	// 	populateNavbar(userName, [], locale);
	// 	populateRecentlyAnnotated();
	// 	domainSettings(domain)
	// 	.then(function(domainData) {
	// 		deferred.resolve(domainData)}
	// 	);
	// 	return deferred.promise();
	// }
	//
	// userLoggedIn()
	// .then(function(loginData) {
	// 	return onLoggedIn(loginData);
	// }, function() {
	// 	// Show login modal if the user is not yet logged in
	// 	var onDismissal = function(){document.location.href="intro.html";};
	// 	return loginModal(onLoggedIn, onDismissal)
	// })
	// .then(function(domainData) {
	// 	ui = domainData.ui + "profile";
	// 	return getLabels(locale, ui);
	// })
	// .then(function(labels) {
	// 	addButtonEvents();
	// 	initLabels(labels);
	// 	initDomains(labels);
	// });
}

function populateRecentlyAnnotated() {
	$.getJSON("annotations", {uri:user, type:"user"})
	.done(function(uris){
		var numberOfItems = uris.length;
		var items = [];

		if(numberOfItems === 0) {
			$("#profileDivLastAnnotated").hide();
		} else {
			for (var i=0; i<numberOfItems; i++) {
				var uri = uris[i];
				items[i] = new item(uri);
			}
			//Create clusters for easy adding based on search.js code
			initialClusters[0] = new cluster([], items);
			enrichedClusters[0] = new cluster([], 'undefined');
			addItems(0);
		}
	});
}

function initLabels(labels) {
	// Add retrieved labels to html elements
	document.title = labels.profilePageTitle;
	// Check if real name is available
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

function initDomains(labels) {
	var onDomains = function(domainLabels){
		populateDomains(domainLabels, labels);
	};
	getAvailableDomains(onDomains);
}

function populateDomains(domainLabels, labels) {
	// Get domain settings for all the domains
	for(var i=0; i<domainLabels.length; i++) {
		var currentDomain = domainLabels[i];
		var processDomain = function(currentDomain, labels){
			// Add title current domain or option to change to domain
			return function(data){
					if(domain===currentDomain) {
						addDomainTitle(data, labels);
					} else {
						domainHtml(data);
					}
			}
		}
		//Add info about all domains except generic
		if(currentDomain !== "generic") {
			$.getJSON("domains", {domain:currentDomain})
			.done(processDomain(currentDomain, labels));
		}
	}
}

function addDomainTitle(domainSettings, labels) {
	// Add the title of the current domain to the profile page
	$.getJSON("ui_elements", {locale:locale,
							  ui:domainSettings.ui + "domain",
							  type:"labels"})
	.done(function(data){
		$("#profileTxtDomain").append(
			labels.profileTxtDomain,
			$.el.span({'class':'text-info'},
				data.domainLabel));});
}

function domainHtml(domainData) {
	var domain = domainData.domain;
	$.getJSON("ui_elements", {locale:locale,
							  ui:domainData.ui + "domain",
							  type:"labels"})
	.done(function(data){
		$("#profileLstDomainItems").append(
			$.el.li(
				$.el.a({'href':'#',
						'id':domainData.domain},
						 data.domainLabel)));
		addDomainEvent(domain);
	});
}

function addDomainEvent(domain) {
	var onSuccess = function(){location.reload();};
	$("#" + domain).click(function() {
		setDomain(domain, onSuccess);
	});
}

function addButtonEvents() {
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
