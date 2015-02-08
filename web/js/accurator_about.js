/* Accurator About
*/
var locale;
var ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#about";


function aboutInit() {
	locale = getLocale();
	onSuccess = function(data){
		populateUI();
		userName = getUserName(data.user);
		populateNavbar(userName, [{link:"/profile.html", name:"Profile"}]);
	};
	onFail = function(){
		populateUI();
	};
	logUserIn(onSuccess, onFail);
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
		.done(function(data){
			initLabels(data);});
}

function initLabels(data) {
	$("#txtAboutSlogan").append(data.txtAboutSlogan);
	$("#txtAboutAccurator").append(data.txtAboutAccurator);
	$("#txtSealincMediaSlogan").append(data.txtSealincMediaSlogan);
	$("#txtAboutSealincMedia").append(data.txtAboutSealincMedia);
	$("#txtSealincResearchSlogan").append(data.txtSealincResearchSlogan);
	$("#txtAboutSealincResearch").append(data.txtAboutSealincResearch);
}