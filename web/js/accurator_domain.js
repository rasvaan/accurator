/* Accurator Domain
*/
var locale, domain;

function domainInit() {
	locale = getLocale();
	domain = getDomain();

	// If user is logged in go to profile page
	onLoggedIn = function() {
		var onDomains = function(data){
			// Continue to next page if already a valid domain is given
			if ((data.indexOf(domain) > -1) && !(domain ==="generic"))
				document.location.href="additional_info.html";
			populateDomains(data);
			populateUI();
		};
		getAvailableDomains(onDomains);
	};
	// If user is not logged go to intro page
	onDismissal = function(){document.location.href="intro.html";};
	logUserIn(onLoggedIn, onDismissal);
}

function populateUI() {
	genericUI = "http://accurator.nl/ui/generic#domain";
	$.getJSON("ui_elements", {locale:locale, ui:genericUI, type:"labels"})
		.done(function(data){
			$("#txtTitle").append(data.txtTitle);
		});
	
}

function populateDomains(domainLabels) {
	domainsToShow = domainLabels.length - 1;
	avaulableCulmns = 12;
	columnWidth = 12/domainsToShow;
	var row;
	
	// Get domain settings for all the domains
	for(i=0; i<domainLabels.length; i++) {
		if(!(i%2===0)) {
			row = parseInt((i/2) + 0.5);
			// Add a new row for every two domains
			$(".domains").append(
				$.el.div({'class':'row',
						  'id':'domain' + row}));
		}

		$.getJSON("domains", {domain:domainLabels[i]})
			.done(function(data){
				if(!(data.domain === "generic")) {
					domainHtml(data, row);
				}
			});
	}
}

function domainHtml(domainData, row) {
	var domain = domainData.domain;
	$.getJSON("ui_elements", {locale:locale,
							  ui:domainData.ui + "domain",
							  type:"labels"})
		.done(function(data){
			console.log(domainData);
			console.log(data);
			$("#domain" + row).append(
				$.el.div({'class':'noPadding col-md-'+columnWidth},
					$.el.h3({'class':'domainTitle',
							 'id':'text' + domain},
							 data.domainLabel),
					$.el.img({'class':'domainImage',
							  'id':'image' + domain,
							  'src':domainData.image})));
			if(domainData.image_brightness === "dark")
				$("#text" + domainData.domain).css('color', '#fff');
			addDomainEvent(domain);
		});
	
	
}

function addDomainEvent(domain) {
	$("#image"+domain).click(function() {
		setDomain(domain);
		document.location.href="additional_info.html";
	});
	$("#text"+domain).click(function() {
		setDomain(domain);
		document.location.href="additional_info.html";
	});
}



