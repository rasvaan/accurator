/* Accurator Results
*/
var query, locale, ui, userName, realName, target;

displayOptions = {
	numberDisplayedItems: 4,
	showFilters: false,
	imageFilter: 'onlyImages'
}

function resultsInit() {
	locale = getLocale();
	domain = getDomain();
	
	onLoggedIn = function(loginData){
		setLinkLogo("profile");
		user = loginData.user;
		userName = getUserName(user);
		realName = loginData.real_name;
		var userParam = getParameterByName("user");
		query = getParameterByName("query");
		
		populateNavbar(userName, [{link:"profile.html",							   name:"Profile"}]);
		
		onDomain = function(domainData) {
			ui = domainData.ui + "results";
			target = domainData.target;
			populateUI();
			addButtonEvents();
			
			//Provide results based on query or recommend something. In case of no in put recommend based on retrieved user.
			if(query != "") {
				initiateSearch(query);
			} else if(userParam != "") {
				query = "expertise values";
				recommendItems(userParam);
			} else {
				query = "expertise values";
				recommendItems(user);
			}
			localStorage.setItem("query", query);
		};
		domainSettings(domain, onDomain);
	};
	onDismissal = function(){document.location.href="intro.html";};
	logUserIn(onLoggedIn, onDismissal);
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui,
							  type:"labels"})
	.done(function(labels){
		initLabels(labels);
	});
}

function initLabels(labels) {
	// Add retrieved labels to html elements
	document.title = labels.title;
	
	$("#btnResultsSearch").append(labels.btnResultsSearch);
	$("#btnResultsRecommend").append(labels.btnResultsRecommend);
}

function addButtonEvents() {
	$("#btnResultsRecommend").click(function() {
		document.location.href="results.html" + "?user=" + user;
	});
	// Search on pressing enter
	$("#frmSearch").keypress(function(event) {
		if (event.which == 13) {
			var query = encodeURIComponent($("#frmSearch").val());
			document.location.href="results.html?query=" + query;
		}
	});
	$("#btnResultsSearch").click(function() {
		var query = encodeURIComponent($("#frmSearch").val());
		document.location.href="results.html?query=" + query;
	});
}

function initiateSearch(query) {
	search(query);
}

function recommendItems(user) {
console.log("target", target);
	$.getJSON("recommendation", {strategy:'expertise',
								 user:user,
								 target:target})
	.done(function(data){
		$("#results").children().remove();
		showFilters();
		processJsonResults(data);
		createResultClusters();
		$(document).prop('title', 'Recommendations for ' + realName);
	})
	.fail(function(data, textStatus){
		$("#results").children().remove();
		$("#results").append(errorHtml(data, textStatus));
		$(document).prop('title', 'Error on ' + query);
	});
}