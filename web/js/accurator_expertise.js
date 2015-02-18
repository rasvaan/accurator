/* Accurator Expertise
*/
var locale, ui, user, domain, domainSettings;
var topics;
var userExpertise = {};
var sldALot, sldNothing;

function expertiseInit() {
	locale = getLocale();
	domain = getDomain();
	
	// Make sure user is logged in
	onLoggedIn = function(loginData){
		setLinkLogo("profile");
		
		//Get domain settings before populating ui
		onDomain = function(domainData) {
			ui = domainData.ui + "expertise";
			populateUI(domainData.taxonomy,
					   domainData.top_concept,
					   domainData.max_topics);
			user = loginData.user;
			var userName = getUserName(user);
			populateNavbar(userName, [{link:"profile.html", name:"Profile"}]);
		};
		domainSettings = domainSettings(domain, onDomain);
	};
	onDismissal = function(){document.location.href="intro.html"};
	logUserIn(onLoggedIn, onDismissal);
}

function populateUI(taxonomy, topConcept, maxTopics) {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
		.done(function(data){
			registerButtonEvent();
			initLabels(data);
			initExpertiseTopics(taxonomy, topConcept, maxTopics);});
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
		processExpertiseValues();
	});
	$("#btnSkip").click(function() {
		document.location.href="profile.html";
	});	
}

function initExpertiseTopics(taxonomy, topConcept, maxTopics) {
	$.getJSON("expertise_topics", {locale:locale,
								   taxonomy:taxonomy,
								   top_concept:topConcept,
								   number_of_topics:maxTopics})
	.done(function(data){
		topics = generateIds(data.topics);
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
									expertiseSlider(topics[i].id),
									$.el.small({'class':'sliderLabel'}, sldALot))));
			initSlider(topics[i].id);
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
										expertiseSlider(topics[i].id),
										$.el.small({'class':'sliderLabel'}, sldALot))));
			initSlider(topics[i].id);
		}
	})
}

function generateIds(topics) {
	for(i=0; i<topics.length; i++) {
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
		    url: "save_expertise_values",
			contentType: "application/json",
			data: JSON.stringify(userExpertise),
			success: function(){
				       document.location.href="profile.html";
			}
	});
}

function printArray(labelArray) {
	var arrayString = "";
	for(var i=0; i<labelArray.length; i++) {
		arrayString += " " + labelArray[i];
	}
	return arrayString;
}