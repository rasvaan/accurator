/* Accurator Expertise
*/
var locale;
var ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#expertise";
var sldALot, sldNothing;

function expertiseInit() {
	// Make sure user is logged in
	onSuccess = function(data){
		locale = getLocale();
		populateUI();
		var userName = getUserName(data.user);
		populateNavbar(userName, [{link:"/profile.html", name:"Profile"}]);
	};
	onDismissal = function(){document.location.href="/intro.html"};
	logUserIn(onSuccess, onDismissal);
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
		.done(function(data){
			registerButtonEvent();
			initLabels(data);
			initExpertiseTopics();});
}

function initLabels(data) {
	$("#txtHeader").prepend(data.txtHeader);
	$("#txtSubHeader").prepend(data.txtSubHeader);
	$("#frmRealName").append(data.frmRealName);
	$("#btnSubmit").append(data.btnSubmit);
	$("#btnSkip").append(data.btnSkip);
	sldALot = data.sldALot;
	sldNothing = data.sldNothing;
}

function registerButtonEvent() {
	$("#btnSubmit").click(function() {
		document.location.href="profile.html";
	});
	$("#btnSkip").click(function() {
		document.location.href="profile.html";
	});	
}

function initExpertiseTopics() {
	$.getJSON("expertise_topics", {locale:locale})
	.done(function(data){
		var topics = data.topics;
		var halfTheTopics = parseInt(topics.length/2, 10);

		for(var i=0; i<halfTheTopics; i++) {
			$("#frmExpertiseLeft").append(
					$.el.div({'class':'row'},
							 $.el.h5(topics[i].label,
									 $.el.small(printArray(topics[i].childrens_labels)))));
			$("#frmExpertiseLeft").append(
					$.el.div({'class':'row'},
							$.el.div({'class':'col-md-11 col-md-offset-1'},
									$.el.small({'class':'sliderLabel'}, sldNothing),
									expertiseSlider(topics[i].label),
									$.el.small({'class':'sliderLabel'}, sldALot))));
			initSlider(topics[i].label);
		}
		for(var i=halfTheTopics; i<topics.length; i++) {
			$("#frmExpertiseRight").append(
					$.el.div({'class':'row'},
							 $.el.h5(topics[i].label,
									 $.el.small(printArray(topics[i].childrens_labels)))));
			$("#frmExpertiseRight").append(
							$.el.div({'class':'row'},
									$.el.div({'class':'col-md-11 col-md-offset-1'},
											$.el.small({'class':'sliderLabel'}, sldNothing),
											expertiseSlider(topics[i].label),
											$.el.small({'class':'sliderLabel'}, sldALot))));
			initSlider(topics[i].label);
		}
		})
	.fail(function(data, textStatus){});
}

function expertiseSlider(id) {
	return $.el.input({'id':id,
						'data-slider-id':'sld'+id,
						'type':'text',
						'data-slider-min':'1',
						'data-slider-max':'5',
						'data-slider-step':'0.1',
						'data-slider-value':'3'});	
}

function initSlider(id) {
	$("#"+id).slider();
}

function printArray(labelArray) {
	var arrayString = "";
	for(var i=0; i<labelArray.length; i++) {
		arrayString += " " + labelArray[i];
	}
	return arrayString;
}