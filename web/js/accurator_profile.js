/*******************************************************************************
Accurator Intro
Code for showing statistical elements on the profile page and allowing the user
to change settings.
*******************************************************************************/
var locale, ui, domain, experiment, user, userName, realName;
var recentItems;
var initialClusters, enrichedClusters, clusters;

displayOptions = {
	numberDisplayedItems: 6,
}

function profileInit() {
	locale = getLocale();
	domain = getDomain();
	experiment = getExperiment();

	populateFlags(locale);

	onLoggedIn = function(loginData){
		setLinkLogo("profile");
		user = loginData.user;
		userName = getUserName(user);
		realName = loginData.real_name;
		populateNavbar(userName, []);
		populateRecentlyAnnotated();

		//Get domain settings before populating ui
		onDomain = function(domainData) {
			ui = domainData.ui + "profile";
			populateUI();
			addButtonEvents();
		};
		domainSettings = domainSettings(domain, onDomain);
	};
	onDismissal = function(){document.location.href="intro.html";};
	logUserIn(onLoggedIn, onDismissal);
}

function populateRecentlyAnnotated() {
	$.getJSON("annotations", {uri:user, type:"user"})
	.done(function(uris){
		var numberOfItems = uris.length;
		var items = [];

		if(numberOfItems === 0) {
			//Hide if nothing is annotated
			$("#rowLastAnnotated").hide();
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

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
	.done(function(labels){
		initLabels(labels);
		initDomains(labels);});
}

function initLabels(labels) {
	// Add retrieved labels to html elements
	document.title = labels.title;
	// Check if real name is available
	if (typeof realName !== 'undefined') {
		$("#txtSlogan").prepend(labels.txtSlogan + " " + realName + " ");
	} else {
		$("#txtSlogan").prepend(labels.txtSlogan);
	}
	$("#txtSubSlogan").prepend(labels.txtSubSlogan);
	$("#txtStartAnnotating").append(labels.txtStartAnnotating);
	$("#btnRecommend").append(labels.btnRecommend);
	$("#btnChangeExpertise").append(labels.btnChangeExpertise);
	$("#btnSearch").append(labels.btnSearch);
	$("#btnDomain").prepend(labels.btnDomain);
	$("#lblLastAnnotated").append(labels.lblLastAnnotated);
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
		$("#txtDomain").append(
			labels.txtDomain,
			$.el.span({'class':'text-info'},
				data.domainLabel));});
}

function domainHtml(domainData) {
	var domain = domainData.domain;
	$.getJSON("ui_elements", {locale:locale,
							  ui:domainData.ui + "domain",
							  type:"labels"})
	.done(function(data){
		$("#domainItems").append(
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
	$("#btnRecommend").click(function() {
		document.location.href="results.html" + "?user=" + user;
	});
	// Search on pressing enter
	$("#frmSearch").keypress(function(event) {
		if (event.which == 13) {
			var query = encodeURIComponent($("#frmSearch").val());
			document.location.href="results.html?query=" + query;
		}
	});
	$("#btnSearch").click(function() {
		var query = encodeURIComponent($("#frmSearch").val());
		document.location.href="results.html?query=" + query;
	});
	$("#btnChangeExpertise").click(function() {
		document.location.href="expertise.html";
	});
}
