/* Accurator Annotate
*/
var locale, domain, ui, uri;

displayOptions = {
	showAnnotations: true,
	metadataLinkBase: 'results.html?query='
}

function annotateInit() {
	locale = getLocale();
	domain = getDomain();
	uri = getParameterByName("uri");
	
	// Make sure user is logged in
	onLoggedIn = function(loginData) {
		setLinkLogo("profile");
		
		//Get domain settings before populating ui
		onDomain = function(domainData) {
			ui = domainData.ui + "annotate";
			populateUI();
			var userName = getUserName(loginData.user);
			populateNavbar(userName, [{link:"profile.html", name:"Profile"}]);
		};
		domainSettings = domainSettings(domain, onDomain);
	};
	// If user is not logged go to intro page
	onDismissal = function(){document.location.href="intro.html";};
	logUserIn(onLoggedIn, onDismissal);
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
		.done(function(labels){
			document.title = labels.title;
		});
	console.log(uri);
	showResult(uri);
}