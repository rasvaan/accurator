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
	
	// Get domain settings for all the domains
	for(i=0; i<domainLabels.length; i++) {
		$.getJSON("domains", {domain:domainLabels[i]})
			.done(function(data){
				if(!(data.domain === "generic")) {
					$("#domains").append(domainHtml(data, columnWidth));
					addDomainEvent(data.domain);
					
					if(data.image_brightness === "dark")
						$("#text" + data.domain).css('color', '#DDD');
				}
			});
	}
}

function domainHtml(data) {
	return $.el.div({'class':'noPadding col-md-'+columnWidth},
		$.el.h3({'class':'domainTitle',
				 'id':'text' + data.domain},
			data.domain),
		$.el.img({'class':'domainImage',
				  'id':'image' + data.domain,
				  'src':data.image}));
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



