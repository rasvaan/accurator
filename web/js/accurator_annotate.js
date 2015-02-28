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
		initLabels(labels);
		addButtonEvents();
	});
	console.log(uri);
	showResult(uri);
}

function initLabels(data) {
	$("#btnPrevious").append(data.btnPrevious);
	$("#btnNext").prepend(data.btnNext);
	$("#btnResultRecommend").append(data.btnResultRecommend);
	$("#btnResultSearch").append(data.btnResultSearch);
}

function addButtonEvents() {
	$("#btnResultRecommend").click(function() {
		//document.location.href="results.html" + "?user=" + user;
	});
	// Search on pressing enter
	$("#frmSearchInput").keypress(function(event) {
		if (event.which == 13) {
			var query = encodeURIComponent($("#frmSearchInput").val());
			document.location.href="results.html?query=" + query;
		}
	});
	$("#btnResultSearch").click(function() {
		var query = encodeURIComponent($("#frmSearch").val());
		document.location.href="results.html?query=" + query;
	});
}