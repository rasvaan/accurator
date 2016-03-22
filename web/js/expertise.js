/*******************************************************************************
Accurator Expertise
Show expertise topics for the selected domain. Expertise topics are retrieved
from triple store and sliders are used for providing values.
*******************************************************************************/
"use strict";

function expertiseInit() {
	var locale = getLocale();
	var domain = getDomain();

	// add language switch to navbar
	populateFlags(locale);

	userLoggedIn()
	.then(function(userData) {
		drawPage(userData);
	}, function() {
		// user is not logged in, show modal
		var onDismissal = function() {document.location.href="intro.html"};
		login(drawPage, onDismissal);
	});

	function drawPage(userData) {
		setLinkLogo("profile");
		var domainData;

		domainSettings(domain)
		.then(function(data) {
			domainData = data;
			var user = userData.user;
			var userName = getUserName(user);
			populateNavbar(userName, [{link:"profile.html", name:"Profile"}], locale);
			return getLabels(locale, domainData.ui + "expertise");
		})
		.then(function(labels) {
			var labelArray = initLabels(labels);
			return initExpertiseTopics(domainData, locale, labelArray)
		})
		.then(function(topics) {
			registerEvents(topics);
		});
	}
}

function initLabels(data) {
	$("#expertiseTxtHeader").prepend(data.expertiseTxtHeader);
	$("#expertiseTxtSubHeader").prepend(data.expertiseTxtSubHeader);
	$("#expertiseBtnSubmit").append(data.expertiseBtnSubmit);
	$("#expertiseBtnSkip").append(data.expertiseBtnSkip);

	var labels = {
		sldALot: data.expertiseSldALot,
		sldNothing: data.expertiseSldNothing,
		txtChangeAll: data.expertiseTxtChangeAll
	};

	return labels;
}

function registerEvents(topics) {
	$("#expertiseBtnSubmit").click(function() {
		processExpertiseValues(topics);
	});
	$("#expertiseBtnSkip").click(function() {
		document.location.href = "results.html";
	});
}

function initExpertiseTopics(domainData, locale, labels) {
	return $.getJSON("expertise_topics", {
		locale:locale,
		taxonomy:domainData.taxonomy,
		top_concept:domainData.top_concept,
		number_of_topics:domainData.number_of_topics,
		number_of_children_shown:domainData.number_of_children_shown
	})
	.then(function(data){
		var topics = generateIds(data.topics);
		var sliderIds = [];
		var halfTheTopics = parseInt(topics.length/2, 10);

		addMasterSlider(sliderIds, labels);

		for(var i = 0; i < halfTheTopics; i++) {
			$("#expertiseDivExpertiseLeft").append(
				$.el.div({'class':'row'},
					$.el.div({'class':'col-md-10 col-md-offset-1'},
						$.el.h5({'id':'expertiseLblExpertise'},
							topics[i].label,
							$.el.small(printArray(topics[i].childrens_labels))))));
			$("#expertiseDivExpertiseLeft").append(
				$.el.div({'class':'row'},
					$.el.div({'class':'col-md-10 col-md-offset-1'},
						$.el.small({'class':'expertiseLblSlider'}, labels.sldNothing),
									expertiseSlider(topics[i].id),
							$.el.small({'class':'expertiseLblSlider'}, labels.sldALot))));
			initSlider(topics[i].id);
			sliderIds[i] = topics[i].id;
		}
		for(var i = halfTheTopics; i < topics.length; i++) {
			$("#expertiseDivExpertiseRight").append(
				$.el.div({'class':'row'},
					$.el.div({'class':'col-md-10 col-md-offset-1'},
						$.el.h5({'id':'expertiseLblExpertise'},
							topics[i].label,
							$.el.small(printArray(topics[i].childrens_labels))))));
			$("#expertiseDivExpertiseRight").append(
				$.el.div({'class':'row'},
					$.el.div({'class':'col-md-10 col-md-offset-1'},
						$.el.small({'class':'expertiseLblSlider'}, labels.sldNothing),
						expertiseSlider(topics[i].id),
						$.el.small({'class':'expertiseLblSlider'}, labels.sldALot))));
			initSlider(topics[i].id);
			sliderIds[i] = topics[i].id;
		}
		setSliderValues();

		return topics;
	});
}

function generateIds(topics) {
	for(var i = 0; i < topics.length; i++) {
		topics[i].id = generateIdFromUri(topics[i].uri);
	}

	return topics;
}

function expertiseSlider(id) {
	return $.el.input({'id':id,
						'data-slider-id':'sld' + id,
						'type':'text',
						'data-slider-min':'1',
						'data-slider-max':'5',
						'data-slider-step':'0.1',
						'data-slider-value':'3'});
}

function initSlider(id) {
	$("#" + id).slider();
}

function processExpertiseValues(topics) {
	var userExpertise = {};
	userExpertise.user = user;
	userExpertise.expertise = {};

	for (var i = 0; i < topics.length; i++) {
		var value = $("#" + topics[i].id).val();
		var scaledValue = (value - 1) / 4;
		var roundedValue = scaledValue.toFixed(2);
		userExpertise.expertise[topics[i].uri] = roundedValue;
	}

	$.ajax({type: "POST",
		    url: "expertise_values",
			contentType: "application/json",
			data: JSON.stringify(userExpertise),
	})
	.then(function() {
		document.location.href = "results.html";
	});
}

function printArray(labelArray) {
	var arrayString = "";

	for(var i = 0; i < labelArray.length; i++) {
		arrayString += " " + labelArray[i];
	}

	return arrayString;
}

function setSliderValues() {
	$.getJSON("expertise_values")
	.then(function(expertValues){
		var uris = Object.keys(expertValues);

		for(var i = 0; i < uris.length; i++){
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

function addMasterSlider(sliderIds, labels) {
	$("#expertiseDivExpertiseMaster").append(
		$.el.div({'class':'row'},
			$.el.div({'class':'col-md-10 col-md-offset-1'},
				$.el.h4({'id':'expertiseLblExpertise'},
					labels.txtChangeAll))));
	$("#expertiseDivExpertiseMaster").append(
		$.el.div({'class':'row'},
			$.el.div({'class':'col-md-10 col-md-offset-1'},
				$.el.span({'id':'expertiseLblMasterLeft'}, labels.sldNothing),
				expertiseSlider("master"),
				$.el.span({'id':'expertiseLblMasterRight'}, labels.sldALot))));
	$("#master").slider();

	//Change all values on using master slider
	$("#master").on("slide", function(slideEvt) {
		for(var i = 0; i < sliderIds.length; i++) {
			$("#" + sliderIds[i]).slider('setValue', slideEvt.value);
		}
	});
}
