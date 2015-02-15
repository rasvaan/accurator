/* Accurator Profile
*/
var locale, domain, user, userName;
var ui = "http://accurator.nl/ui/bird#profile";
var recentItems;
var initialClusters, enrichedClusters, clusters;

displayOptions = {
		numberDisplayedItems: 6,
}

function profileInit() {
	locale = getLocale();
	domain = getDomain();
	
	onLoggedIn = function(data){
		setLinkLogo("profile");
		user = data.user;
		userName = getUserName(user);
		populateUI();
		addButtonEvents();
		populateNavbar(userName, []);
		getRecentlyAnnotated();
	};
	onDismissal = function(){document.location.href="intro.html";};
	logUserIn(onLoggedIn, onDismissal);
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
		.done(function(data){
			initLabels(data);
			initLocaleRadio();
			initDomains(data);})
		.fail(function(data, textStatus){
			$("#txtSubSlogan").replaceWith('Problem connecting to server, please contact the system administrator');});
}

function initLocaleRadio() {
	if (locale === "en") {
		$("#radioLocaleEn").trigger('click');
	} else {
		$("#radioLocaleEn").click(function() {
			setLocale("en");
			location.reload();
		});
	}
	if (locale === "nl") {
		$("#radioLocaleNl").trigger('click');
	} else {
		$("#radioLocaleNl").click(function() {
			setLocale("nl");
			location.reload();
		});
	}
}

function initDomains(textLabels) {
	var onDomains = function(data){
		populateDomains(data, textLabels);
	};
	getAvailableDomains(onDomains);
}

function populateDomains(domainLabels, textLabels) {
	// Get domain settings for all the domains
	for(i=0; i<domainLabels.length; i++) {
		var currentDomain = domainLabels[i];
		var processDomain = function(currentDomain, textLabels){
			return function(data){
					if(domain===currentDomain) {
						addDomainTitle(data, textLabels);
					} else {
						domainHtml(data);
					}
			}
		}
		//Add info about all domains except generic
		if(currentDomain !== "generic") {
			$.getJSON("domains", {domain:currentDomain})
				.done(processDomain(currentDomain, textLabels));
		}
	}
}

function addDomainTitle(domainSettings, textLabels) {
	$.getJSON("ui_elements", {locale:locale,
							  ui:domainSettings.ui + "domain",
							  type:"labels"})
		.done(function(data){
			$("#txtDomain").append(
				textLabels.txtDomain +
				data.domainLabel);
		});
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
	$("#" + domain).click(function() {
		setDomain(domain);
		location.reload();
	});
}

function initLabels(data) {
	$("#txtSlogan").prepend(data.txtSlogan + " " + userName);
	$("#txtSubSlogan").prepend(data.txtSubSlogan);
	$("#txtStartAnnotating").append(data.txtStartAnnotating);
	$("#btnRecommend").append(data.btnRecommend);
	$("#btnChangeExpertise").append(data.btnChangeExpertise);
	$("#btnChangeInfo").append(data.btnChangeInfo);
	$("#btnSearch").append(data.btnSearch);
	$("#btnDomain").prepend(data.btnDomain);
	$("#lblLastAnnotated").append(data.lblLastAnnotated);
	$("#frmChangeLocale").append(data.frmChangeLocale);
	$("#radioLocaleEn").after(data.radioLocaleEn);
	$("#radioLocaleNl").after(data.radioLocaleNl);
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
	$("#btnChangeInfo").click(function() {
		document.location.href="additional_info.html";
	});
}

function getRecentlyAnnotated() {
	$.getJSON("recently_annotated", {user:user})
		.done(function(data){
			var numberOfItems = data.uris.length;
			var items = [];

			if(numberOfItems === 0) {
				$("#rowLastAnnotated").hide();
			} else {
				for (var i=0; i<numberOfItems; i++) {
					var uri = data.uris[i];
					items[i] = new item(uri);
				}
				initialClusters[0] = new cluster([], items);
				enrichedClusters[0] = new cluster([], 'undefined');
				addItems(0);
			}
		});
}