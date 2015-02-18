/* Accurator Results
*/
var locale, ui, userName;

displayOptions = {
	numberDisplayedItems: 4,
	showFilters: false,
	imageFilter: 'onlyImages'
}

function resultsInit() {
	locale = getLocale();
	domain = getDomain();
	
	onLoggedIn = function(data){
		setLinkLogo("profile");
		user = data.user;
		userName = getUserName(user);
		populateNavbar(userName, [{link:"profile.html", name:"Profile"}]);
		var query = getParameterByName("query");
		var userParam = getParameterByName("user");
		
		//Provide results based on query or recommend something. In case of no in put recommend based on retrieved user.
		if(query != "") {
			initiateSearch(query);
		} else if(userParam != "") {
			recommendItems(userParam);
		} else {
			recommendItems(user);
		}
		
		onDomain = function(domainData) {
			ui = domainData.ui + "results";
			populateUI();
			addButtonEvents();
		};
		domainSettings(domain, onDomain);
	};
	onDismissal = function(){document.location.href="intro.html";};
	logUserIn(onLoggedIn, onDismissal);
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
	.done(function(labels){ initLabels(labels); });
}

function initLabels(labels) {
	// Add retrieved labels to html elements
	document.title = labels.title;
	$("#btnSearch").append(labels.btnSearch);
}

function addButtonEvents() {
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
}

function initiateSearch(query) {
	search(query);
}

function recommendItems(user) {
	alert("Should be recommending items to  " + user);
}