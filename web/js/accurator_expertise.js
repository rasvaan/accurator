/*******************************************************************************
Accurator Expertise
Show expertise topics for the selected domain. Expertise topics are retrieved
from triple store and sliders are used for providing values.
*******************************************************************************/
var locale, ui, user, domain, experiment, domainSettings;
var topics;
var userExpertise = {};
var sldALot, sldNothing, txtChangeAll, lblDomain;
var sliderIds = [];

function expertiseInit() {
	// Get settings
	locale = getLocale();
	domain = getDomain();
	experiment = getExperiment();

	// Add language switch to navbar
	populateFlags(locale);

	// Make sure user is logged in
	onLoggedIn = function(loginData){
		setLinkLogo("profile");

		//Get domain settings before populating ui
		onDomain = function(domainData) {
			ui = domainData.ui + "expertise";
			populateUI(domainData);
			user = loginData.user;
			var userName = getUserName(user);
			populateNavbar(userName, [{link:"profile.html", name:"Profile"}]);
		};
		domainSettings = domainSettings(domain, onDomain);
	};
	onDismissal = function(){document.location.href="intro.html"};
	logUserIn(onLoggedIn, onDismissal);
}

function nextPage() {
	return function(){document.location.href="results.html"};
}

function populateUI(domainData) {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
	.done(function(data){
		registerEvents();
		initLabels(data);
		initExpertiseTopics(domainData);});
}

function initLabels(data) {
	$("#txtHeader").prepend(data.txtHeader);
	$("#txtSubHeader").prepend(data.txtSubHeader);
	$("#btnSubmit").append(data.btnSubmit);
	$("#btnSkip").append(data.btnSkip);
	sldALot = data.sldALot;
	sldNothing = data.sldNothing;
	lblDomain = data.lblDomain;
	txtChangeAll = data.txtChangeAll;
}

function registerEvents() {
	$("#btnSubmit").click(function() {processExpertiseValues();});
	$("#btnSkip").click(nextPage());
}

function initExpertiseTopics(domainData) {
	$.getJSON("expertise_topics", {
		locale:locale,
		taxonomy:domainData.taxonomy,
		top_concept:domainData.top_concept,
		number_of_topics:domainData.number_of_topics,
		number_of_children_shown:domainData.number_of_children_shown
	})
	.done(function(data){
		topics = generateIds(data.topics);
		var halfTheTopics = parseInt(topics.length/2, 10);

		addMasterSlider();

		for(var i=0; i<halfTheTopics; i++) {
			$("#frmExpertiseLeft").append(
				$.el.div({'class':'row'},
					$.el.div({'class':'col-md-10 col-md-offset-1'},
						$.el.h5({'id':'expertiseLabel'},
							topics[i].label,
							$.el.small(printArray(topics[i].childrens_labels))))));
			$("#frmExpertiseLeft").append(
				$.el.div({'class':'row'},
					$.el.div({'class':'col-md-10 col-md-offset-1'},
						$.el.small({'class':'sliderLabel'}, sldNothing),
									expertiseSlider(topics[i].id),
							$.el.small({'class':'sliderLabel'}, sldALot))));
			initSlider(topics[i].id);
			sliderIds[i] = topics[i].id;
		}
		for(var i=halfTheTopics; i<topics.length; i++) {
			$("#frmExpertiseRight").append(
				$.el.div({'class':'row'},
					$.el.div({'class':'col-md-10 col-md-offset-1'},
						$.el.h5({'id':'expertiseLabel'},
							topics[i].label,
							$.el.small(printArray(topics[i].childrens_labels))))));
			$("#frmExpertiseRight").append(
				$.el.div({'class':'row'},
					$.el.div({'class':'col-md-10 col-md-offset-1'},
						$.el.small({'class':'sliderLabel'}, sldNothing),
						expertiseSlider(topics[i].id),
						$.el.small({'class':'sliderLabel'}, sldALot))));
			initSlider(topics[i].id);
			sliderIds[i] = topics[i].id;
		}
		setSliderValues();
	});
}

function generateIds(topics) {
	for(var i=0; i<topics.length; i++) {
		topics[i].id = generateIdFromUri(topics[i].uri);
	}
	return topics;
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

function processExpertiseValues() {
	userExpertise.user = user;
	userExpertise.expertise = {};
	for (var i=0; i<topics.length; i++) {
		var value = $("#"+topics[i].id).val();
		var scaledValue = (value - 1) / 4;
		var roundedValue = scaledValue.toFixed(2);
		userExpertise.expertise[topics[i].uri] = roundedValue;
	}
	$.ajax({type: "POST",
		    url: "expertise_values",
			contentType: "application/json",
			data: JSON.stringify(userExpertise),
			success: nextPage()
	});
}

function printArray(labelArray) {
	var arrayString = "";
	for(var i=0; i<labelArray.length; i++) {
		arrayString += " " + labelArray[i];
	}
	return arrayString;
}

function setSliderValues() {
	$.getJSON("expertise_values")
	.done(function(expertValues){
		var uris = Object.keys(expertValues);
		for(var i=0; i<uris.length; i++){
			// Generate id based on uri
			var uri = uris[i];
			var id = generateIdFromUri(uri);
			var value = Number(expertValues[uris[i]]);
			var descaledValue = (value * 4) + 1;

			// if no expertise value is given, set the default to 3
			if(expertValues[uris[i]] === "-1")
				descaledValue = 3;

			// Set slider value
			$("#"+id).slider('setValue', descaledValue);
		}
	});
}

function addMasterSlider() {
	$("#frmExpertiseMaster").append(
		$.el.div({'class':'row'},
			$.el.div({'class':'col-md-10 col-md-offset-1'},
				$.el.h4({'id':'expertiseLabel'},
					txtChangeAll))));
	$("#frmExpertiseMaster").append(
		$.el.div({'class':'row'},
			$.el.div({'class':'col-md-10 col-md-offset-1'},
				$.el.span({'id':'masterLabelLeft'}, sldNothing),
				expertiseSlider("master"),
				$.el.span({'id':'masterLabelRight'}, sldALot))));
	$("#master").slider();

	//Change all values on using master slider
	$("#master").on("slide", function(slideEvt) {
		for(var i=0; i<sliderIds.length; i++) {
			$("#" + sliderIds[i]).slider('setValue', slideEvt.value);
		}
	});
}
