/*******************************************************************************
Accurator Search
This code loads possible search queries into the page, where the search options
depend on the search queries loaded in the triple store.
*******************************************************************************/
var locale, domain, experiment, ui;

function searchInit() {
	locale = getLocale();
	// Be domain agnostic on domain selection screen
	domain = "generic";
	experiment = getExperiment();

	// Add language switch to navbar
	populateFlags(locale);

	onLoggedIn = function(loginData) {
		setLinkLogo("profile");
		onDomains = function(data){
			//populateDomains(data);

			// Get generic domain settings before populating ui
			onDomain = function(domainSettings) {
				ui = getUI(domainSettings, "search");
				populateUI();
				var userName = getUserName(loginData.user);
				populateNavbar(userName, [{link:"profile.html", name:"Profile"}]);
			};
			domainSettings(domain, onDomain);
		};
		// Get a list of the available domain (utilities function)
		//getAvailableDomains(onDomains);
	};
	// If user is not logged go to intro page
	onDismissal = function(){document.location.href="intro.html";};
	logUserIn(onLoggedIn, onDismissal);
}

function nextPage() {
	return function(){document.location.href="expertise.html"};
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
		.done(function(labels){
			document.title = labels.searchPageTitle;
			$("#navbarBtnSearch").append(labels.navbarBtnSearch);
			$("#searchHdrSlogan").append(labels.searchHdrSlogan);
		});
}

// function populateDomains(domainLabels) {
// 	var row;
//
// 	// Get domain settings for all the domains
// 	for(var i=0; i<domainLabels.length; i++) {
// 		if(!(i%2===0)) {
// 			row = parseInt((i/2) + 0.5);
// 			// Add a new row for every two domains
// 			$(".domainDiv").append(
// 				$.el.div({'class':'row',
// 						  'id':'domain' + row}));
// 		}
//
// 		// Add domain specific html to rows
// 		$.getJSON("domains", {domain:domainLabels[i]})
// 			.done(function(data){
// 				if(!(data.domain === "generic")) {
// 					domainHtml(data, row);
// 				}
// 			});
// 	}
// }

// function domainHtml(domainData, row) {
// 	var domain = domainData.domain;
// 	$.getJSON("ui_elements",
// 			  {locale:locale,
// 			   ui:domainData.ui + "domain",
// 			   type:"labels"})
// 		.done(function(data){
// 			$("#domain" + row).append(
// 				$.el.div({'class':'noPadding col-md-6'},
// 					$.el.h3({'class':'domainHdr',
// 							 'id':'domainTxt' + domain},
// 							 data.domainLabel),
// 					$.el.img({'class':'domainImg',
// 							  'id':'domainImg' + domain,
// 							  'src':domainData.image})));
// 			if(domainData.image_brightness === "dark")
// 				$("#domainTxt" + domainData.domain).css('color', '#fff');
// 			addDomainEvent(domain);
// 		});
// }
//
// function addDomainEvent(domain) {
// 	$("#domainImg"+domain).click(function() {
// 		setDomain(domain, nextPage());
// 	});
// 	$("#domainTxt"+domain).click(function() {
// 		setDomain(domain, nextPage());
// 	});
//}
