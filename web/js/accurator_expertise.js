/* Accurator Expertise
*/
var locale = "en";
var ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#expertise";

function expertiseInit() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
	.done(function(data){
		registerButtonEvent();
		initLabels(data);
		initExpertiseTopics();})
	.fail(function(data, textStatus){});
}

function initLabels(data) {
	$("#txtHeader").prepend(data.txtHeader);
	$("#txtSubHeader").prepend(data.txtSubHeader);
	$("#frmRealName").append(data.frmRealName);
	$("#btnSubmit").append(data.btnSubmit);
	$("#btnSkip").append(data.btnSkip);
}

function registerButtonEvent() {
	$("#btnSubmit").click(function() {
		document.location.href="/profile.html";
	});
	$("#btnRegister").click(function() {
		document.location.href="/profile.html";
	});	
}

function initExpertiseTopics() {
	$.getJSON("expertise_topics", {locale:locale})
	.done(function(data){
		var topics = data.topics;
		var halfTheTopics = parseInt(topics.length/2, 10) + 1;

		for(var i=0; i<halfTheTopics; i++) {
			$("#frmExpertiseLeft").append(
					$.el.div({'class':'row'},
							 $.el.h4(topics[i]),
							 expertiseSlider(topics[i])));
			initSlider(topics[i]);
		}
		for(var i=halfTheTopics; i<topics.length; i++) {
			$("#frmExpertiseRight").append(
					$.el.div({'class':'row'},
							 $.el.h4(topics[i]),
							 expertiseSlider(topics[i])));
			initSlider(topics[i]);
		}
		})
	.fail(function(data, textStatus){});
}

function expertiseSlider(id) {
	return $.el.input({'id':id,
						'data-slider-id':'sld'+id,
						'type':'text',
						'data-slider-min':'0',
						'data-slider-max':'1',
						'data-slider-step':'0.05',
						'data-slider-value':'0'});	
}

function initSlider(id) {
	$("#"+id).slider();
}