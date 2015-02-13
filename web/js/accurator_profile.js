/* Accurator Profile
*/
var user, userName;
var locale;
var ui = "http://accurator.nl/ui/bird#profile";
var recentItems;
var initialClusters, enrichedClusters, clusters;
var

displayOptions = {
		numberDisplayedItems: 6,
}

function profileInit() {
	onLoggedIn = function(data){
		setLinkLogo("profile");
		locale = getLocale();
		user = data.user;
		userName = getUserName(user);
		populateUI();
		initLocaleRadio();
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
			initLabels(data);})
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

function initLabels(data) {
	$("#txtSlogan").prepend(data.txtSlogan + " " + userName);
	$("#txtSubSlogan").prepend(data.txtSubSlogan);
	$("#txtStartAnnotating").append(data.txtStartAnnotating);
	$("#btnRecommend").append(data.btnRecommend);
	$("#btnChangeExpertise").append(data.btnChangeExpertise);
	$("#btnChangeInfo").append(data.btnChangeInfo);
	$("#btnSearch").append(data.btnSearch);
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