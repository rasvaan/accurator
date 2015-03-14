/* Accurator Results
*/
var locale, ui, userName, realName;

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
		var query = getParameterByName("query");
		
		populateNavbar(userName, [{link:"profile.html",							   name:"Profile"}]);
		
		onDomain = function(domainData) {
			ui = domainData.ui + "results";
			var target = domainData.target;
			populateUI();
			addButtonEvents();
			
			//Provide results based on query or recommend something. In case of no in put recommend based on retrieved user.
			if(query != "") {
				initiateSearch(query, target);
			} else if(userParam != "") {
				query = "expertise values";
				recommendItems(userParam, query, target);
			} else {
				query = "expertise values";
				recommendItems(user, query, target);
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

function initiateSearch(query, target) {
	search(query, target);
}

function recommendItems(user, query, target) {
	$.getJSON("recommendation", {strategy:'expertise',
								 user:user,
								 target:target})
	.done(function(data){
		setGlobalQuery(query)
		$("#results").children().remove();
		showFilters();
		processJsonResults(data);
		createResultClusters();
		$(document).prop('title', 'Recommendations for ' + realName);
		//Also get a row of random items not yet annotated
		populateRandom(target, data.clusters.length);
	})
	.fail(function(data, textStatus){
		$("#results").children().remove();
		$("#results").append(errorHtml(data, textStatus));
		$(document).prop('title', 'Error on ' + query);
	});
}

function populateRandom(target, clusterIndex) {
	$("#results").append(clusterContainer(clusterIndex));
	$.getJSON("recommendation", {strategy:'random',
								 number:20,
								 target:target})
	.done(function(data){
		var numberOfItems = data.length;
		var items = [];

		for (var i=0; i<numberOfItems; i++) {
			var uri = data[i];
			items[i] = new item(uri);
		}
		initialClusters[clusterIndex] = new cluster([], items);
		enrichedClusters[clusterIndex] = new cluster([], 'undefined');
		$("#cluster"+clusterIndex).prepend(
			$.el.h4(
				$.el.span({'class':'path-label path-literal'},
					"random objects")));
		addItems(clusterIndex);
	});
}
