/* Accurator Expertise
*/
var locale;
var ui = "http://accurator.nl/ui/bird#expertise";
var topics, userExpertise;
var sldALot, sldNothing;

function expertiseInit() {
	// Make sure user is logged in
	onSuccess = function(data){
		setLinkLogo("profile");
		locale = getLocale();
		populateUI();
		var userName = getUserName(data.user);
		populateNavbar(userName, [{link:"profile.html", name:"Profile"}]);
	};
	onDismissal = function(){document.location.href="intro.html"};
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
		saveExpertiseValues();
		//document.location.href="profile.html";
	});
	$("#btnSkip").click(function() {
		document.location.href="profile.html";
	});	
}

function initExpertiseTopics() {
	$.getJSON("expertise_topics", {locale:locale})
	.done(function(data){
		topics = data.topics;
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

function saveExpertiseValues() {
	for (var i=0; i<topics.length; i++) {
		var value = $("#"+topics[i].label).val();
		var scaledValue = (value - 1) / 4;
		var roundedValue = scaledValue.toFixed(2);
		console.log(topics[i].uri, roundedValue);
	}
}

function printArray(labelArray) {
	var arrayString = "";
	for(var i=0; i<labelArray.length; i++) {
		arrayString += " " + labelArray[i];
	}
	return arrayString;
}