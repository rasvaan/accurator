/* Accurator Profile
*/
var user;
var locale;
var ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#profile";
var recentItems;
var initialClusters, enrichedClusters, clusters;

displayOptions = {
		numberDisplayedItems: 6,
}

function profileInit() {
	onSuccess = function(){
		locale = getLocale();
		getUserInfo();
		populateUI();
		initLocaleRadio();
		addButtonEvents();
	};
	onFail = function(){
		document.location.href="intro.html";
	};
	userLoggedIn(onSuccess, onFail);
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
	.done(function(data){
		  initLabels(data);})
	.fail(function(data, textStatus){
		  $("#txtSubSlogan").replaceWith('Problem connecting to server, please contact the system administrator');});
}

function getUserInfo() {
	//get the user id
	$.getJSON("get_user")
	.done(function(data){
		  user = data.user;
		  alert("Logged in user " + user);
		  getRecentlyAnnotated();
		  })
	.fail(function(data, textStatus){
		  
		  });
	return user;
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
	$("#txtSlogan").prepend(data.txtSlogan + " " + user);
	$("#txtSubSlogan").prepend(data.txtSubSlogan);
	$("#txtStartAnnotating").append(data.txtStartAnnotating);
	$("#txtChangeSettings").append(data.txtChangeSettings);
	$("#btnRecommend").append(data.btnRecommend);
	$("#btnChangeExpertise").append(data.btnChangeExpertise);
	$("#btnSearch").append(data.btnSearch);
	$("#lblLastAnnotated").append(data.lblLastAnnotated);
	$("#frmChangeLocale").append(data.frmChangeLocale);
	$("#radioLocaleEn").after(data.radioLocaleEn);
	$("#radioLocaleNl").after(data.radioLocaleNl);
}

function addButtonEvents() {
	$("#btnRecommend").click(function() {
		document.location.href="recommendations.html";
	});
	$("#btnChangeExpertise").click(function() {
		document.location.href="expertise.html";
	});	
}

function getRecentlyAnnotated() {
	$.getJSON("recently_annotated", {user:user})
	.done(function(data){
			var numberOfItems = data.uris.length;
			var items = [];

			if(numberOfItems === 0) {
				$("#cluster0").append(noResultsHtml());
			} else {
				for (var i=0; i<numberOfItems; i++) {
					var uri = data.uris[i];
					items[i] = new item(uri);
				}
				initialClusters[0] = new cluster([], items);
				enrichedClusters[0] = new cluster([], 'undefined');
				addItems(0);
			}
		})
	.fail(function(data, textStatus){});
}

function noResultsHtml() {
	return $.el.h5('You should start annotating to see some results!');
}