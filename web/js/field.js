/*******************************************************************************
Field
Class of annotation fields. Fields are bound to a specific image and annotorious
instance. Most of the code comes from annotate.js written by
Jacco van Ossenbruggen
*******************************************************************************/
function Field(defenition, context) {
	console.log("1.3.1 Field, Construct with defenition: ", defenition, " and context ", context);
	this.id = context.id; // id of field serving as a basis for jquery identifiers
	this.inputId = "itemInp" + context.id; // Id of input
	this.divId = "itemDiv" + context.id; // Id of form group
	this.imageId = context.imageId; // Id of the corresponding img element
	this.fieldsId = context.fieldsId;  // id of the corresponding fields container
	this.annotationsId = context.annotationsId; // id of the container for annotations
	this.field = defenition.uri; // URI identifying annotation field
	this.type = defenition.type; // type of input field (e.g. dropdown or radiobutton)
	this.label = defenition.label; // name of the field
	this.comment = defenition.comment; // short description of the field
	this.source = defenition.source; // source of the alternatives shown
	this.target = context.target; // URI of target to be annotated
	this.targetImage = context.targetImage; // URI of target's image to be annotated
	this.user = context.user; // URI of the user currently annotating
	this._anno = anno; // Jacco hack to get to annotourious
	this.node = null; // html node representing the field
	this.alternatives = null; // list of alternatives for dropdown
	this.annotationList = null; // Array of annotations related to this target and field
	this.showAnnotations = true; // Boolean indicating whether previous annotations should be shown

	this.MOTIVATION = {
		tagging:    'http://www.w3.org/ns/oa#tagging',
		commenting: 'http://www.w3.org/ns/oa#commenting',
		moderating: 'http://www.w3.org/ns/oa#moderating',
	};

	switch (defenition.type) {
		case "DropdownField":
			this.initDropdown();
			break;
	}
}

Field.prototype.initDropdown = function() {
	var _field = this; //make sure we can use this Field in $ scope
	console.log("1.3.2 InitDropdown, generate dom node");
	this.node = this.dropdownField();

	// Get already existing annotations for field
	if(this.showAnnotations) {
		console.log("1.3.3 InitDropdown, create dom annotation list for field ", this.annotationsId);
		this.annotationList = new AnnotationList(this.id + "Annotations");
		console.log("1.3.4 InitDropdown, Add annotation dom element after this element " + this.divId);
		$(this.node).append(this.annotationList.node);
		console.log("1.3.5 InitDropdown, Get already existing annotations for field ", this.field, " and target ", this.target);
		this.getAnnotations();
	}

	console.log("1.3.6 InitDropdown, Get dropdown alternatives");
	this.getAllAlternatives()
	.then(function(alternatives){
		_field.addTypeAhead(alternatives);
		_field.addDropdownListeners();
	});
}

Field.prototype.submitAnnotation = function(motiv, target, body, label, graph) {
	var _field = this; //make sure we can use this Field in $ scope
	if (!target) return; // annotation target is required
	if (!body) return; // annotation in the form of text or resource is required
	if (!label && body['@value']) label = body['@value']; // set label to value in body if not sepperately defined
	if (!motiv) motiv = this.MOTIVATION.tagging;
	if (!graph)	graph = target;

	var targetObject = null;
	if (this._anno && this._anno._deniche.currentShape) {
		var shape = this._anno._deniche.currentShape.geometry;
		var targetImage = this.targetImage;

		if (targetImage && target != targetImage) {
			// Another annotation on selector with existing id (target id is the selector, not the image)
			targetObject = [{hasSource:targetImage, hasSelector:{value:shape}}, {'@id':target}];
		} else {
			// Annotation on new selector, id will be generated server-side
			targetObject = [{hasSource:target, hasSelector:{value:shape}}];
		}
	} else {
		// Annotation without fragment, on entire target image
		targetObject = [{'@id':target}];
	}

	var targetString = JSON.stringify(targetObject);
	var bodyString = JSON.stringify(body);

	console.log("Saving the following ", "field: ", this.field, "hasTarget: ", targetString, "hasBody: ", bodyString, "label: ", label, "motivatedBy: ", motiv, "graph: ", graph);

	$.ajax({type: "POST",
			url: "api/annotation/add",
			data: {
				field:this.field,
				hasTarget:targetString,
				hasBody:bodyString,
				label:label,
			   	motivatedBy: motiv,
				graph:graph
			},
			success: function(data) {
				//Add annotation to list of annotations
				_field.annotationList.add(data.annotation);
				_field.annotationList.render();
			}
	});
}

Field.prototype.getAnnotations = function() {
	var _field = this; //make sure we can use this Field in $ scope
	var annotationPromise =
		$.getJSON("api/annotation/get", {field:this.field, hasTarget:this.target});

	annotationPromise.then(function(data){
		// Get the annotations from the returned data
		var annotations = data[_field.field].annotations;
		var length = annotations.length;

		console.log("1.3.4.1 getAnnotations, iterate through annotations ", annotations);
		for (key in annotations) {
			_field.annotationList.add(annotations[key]);
			_field.addAnnotationFragment(annotations[key], true);
		}
	});
}

Field.prototype.addAnnotationFragment = function(annotation, update) {
	console.log("1.3.4.1.2 addAnnotationFragment, reconstruction annotatoin update: ", update);
	var target = this.annotationList.findSpecificTarget(annotation);
	if (!this._anno || !target) return;

	var label = annotation.title;
	var x = target.hasSelector.x;
	var y = target.hasSelector.y;
	var w = target.hasSelector.w;
	var h = target.hasSelector.h;
	var torious = {
		src: $("#" + this.imageId)[0].src,
		text: label,
		targetId: target['@id'],
		fieldsId: this.fieldsId,
		annotationId: annotation.annotation,
		shapes: [{
			type:'rect',
			geometry: { x:x,y:y,width:w,height:h }
		}]
	};
	console.log("1.3.4.1.3 addAnnotationFragment, add annotation as fragment with info: ", torious);
	this._anno._deniche.addAnnotation(torious, update);
}

Field.prototype.addDropdownListeners = function() {
	var _field = this; //make sure we can use this Field in $ scope
	var selector = "#" + this.inputId;

	// Eventlistener for selecting typeahead alternative
	$(selector).on('typeahead:select', function(event, annotation) {
		// Code for clearing query
		$(selector).typeahead('val', '');

		console.log("SAVE: resource EVENT: typeahead:select ANNOTATION: ", annotation);
		_field.submitAnnotation(
			_field.MOTIVATION.tagging,
			_field.target,
			{'@id':annotation.uri},
			annotation.value
		);
	});

	// Action upon pressing enter
	$(selector).on('keyup', function(event) {
		// Check to see if typeahead cleared the field (so autocomplete was used)
		if ($(selector).val() && event.which == 13) {
			var annotation = $(selector).val();

			console.log("SAVE: literal EVENT: keyup ANNOTATION: ", annotation);
			_field.submitAnnotation(
				_field.MOTIVATION.tagging,
				_field.target,
				{'@value':annotation},
				annotation
			);
			// Clear input
			$(selector).typeahead('val', '');
		}
	});
}

Field.prototype.getAllAlternatives = function() {
	// Get autocomplete alternatives
	var filter = JSON.stringify({scheme:"http://accurator.nl/bible#BiblicalFigureConceptScheme"});
	var labelRank = "['http://www.w3.org/2004/02/skos/core#prefLabel'-1]";

	// Return promise
	return $.getJSON("api/autocomplete",
		{q:"stub",
		 filter:filter,
		 labelrank:labelRank,
		 method:"all",
		 locale:locale});
}

Field.prototype.getAlternatives = function(string) {
	// Get autocomplete alternatives
	var filter = JSON.stringify({scheme:"http://accurator.nl/bible#BiblicalFigureConceptScheme"});
	var labelRank = "['http://www.w3.org/2004/02/skos/core#prefLabel'-1]";

	// Return promise
	return $.getJSON("api/autocomplete",
		{q:string,
		 filter:filter,
		 labelrank:labelRank,
		//  method:"all",
		 locale:locale});
}

Field.prototype.addTypeAhead = function(alternatives) {
	this.alternatives = alternatives;
	var array = [];

	// Prep the data for adding it to the suggestion engine
	for(var i=0; i<alternatives.results.length; i++) {
		array[i] = {
			value:alternatives.results[i].label,
			uri:alternatives.results[i].uri
		};
	}

	console.log("1.3.6.1 addTypeAhead, setup bloodhound with first alternative ", array[0]);
	// Constructs the suggestion engine
	var bloodHoundAlternatives = new Bloodhound({
		datumTokenizer: Bloodhound.tokenizers.obj.whitespace('value'),
		queryTokenizer: Bloodhound.tokenizers.whitespace,
		local: array
	});

	// Create the suggestion template
	var suggestionTemplate = function(data) {
		return '<div>' + data.value + ' - <small>' + data.uri + '</small></div>';
	}

	console.log("1.3.6.2 addTypeAhead, add typeahead to id ", this.inputId);
	// Select the input field and add typeahead
	$("#" + this.inputId).typeahead({hint: true,
						 highlight: true,
						 minLength: 1},
			   			{name:'alternatives',
						 display:'value',
						 source: bloodHoundAlternatives,
						 templates: {
							 suggestion:suggestionTemplate
						 }
	});
}

Field.prototype.dropdownField = function() {
	// Return the form group
	return	$.el.div({'class':'form-group', 'id':this.divId},
				$.el.label({'class':'itemLbl',
							'for':this.inputId},
						   this.label),
				$.el.input({'type':'text',
							'class':'form-control typeahead',
							'id':this.inputId,
							'placeholder':this.comment}));
}

// function annotationField(field) {
// 	switch (field.type) {
// 		case "DropdownField":
// 			var field = dropdownField(id, label, comment);
// 			addAutocompleteDropdown(id, field);
// 			return field;
// 		case "TextField":
// 			return textField(id, label, comment);
// 		case "RadioButtonField":
// 			return radioButtonField(id, label, comment, field.source);
// 		case "CheckboxField":
// 			return checkBoxField(id, label, comment, field.source);
// 		case "SelectField":
// 			return selectField(id, label, comment, field.source);
// 	}
// }

// function dropdownField(id, label, comment) {
// 	return	$.el.div({'class':'form-group'},
// 				$.el.label({'class':'itemLbl',
// 							'for':'itemInp' + id,
// 							'id':'itemLbl' + id},
// 						   label),
// 				$.el.input({'type':'text',
// 							'class':'form-control typeahead',
// 							'id':'itemInp' + id,
// 							'placeholder':comment})
// 	);
// }

// function selectField(id, label, comment, source) {
// 	return	$.el.div({'class':'form-group'},
// 				$.el.label({'class':'itemLbl',
// 							'for':'itemInp' + id,
// 							'id':'itemLbl' + id},
// 						   label),
// 				$.el.select({'class':'form-control',
// 							 'id':'itemSlt' + id,
// 							 'placeholder':comment},
// 						 	 options(source, id))
// 	);
// }
//
// function options(source, fieldId) {
// 	var options = [];
// 	var id = "itemOpt" + fieldId;
//
// 	for(var i=0; i<source.length; i++)
// 		options[i] = $.el.option(source[i]);
// 	return options;
// }
//
//
//
//
//
// function textField(id, label, comment) {
// 	return	$.el.div({'class':'form-group'},
// 				$.el.label({'class':'itemLbl',
// 							'for':'itemInp' + id,
// 							'id':'itemLbl' + id},
// 						   label),
// 				$.el.textarea({'type':'text',
// 							   'class':'form-control',
// 							   'id':'itemInp' + id,
// 							   'rows':'2',
// 							   'placeholder':comment})
// 	);
// }
//
// function radioButtonField(id, label, comment, source) {
// 	return	$.el.div({'class':'form-group'},
// 				$.el.label({'class':'itemLbl',
// 							'for':'itemInp' + id,
// 							'id':'itemLbl' + id},
// 						   label),
// 				buttons(source, id, "radio")
// 	);
// }
//
// function checkBoxField(id, label, comment, source) {
// 	return	$.el.div({'class':'form-group'},
// 				$.el.label({'class':'itemLbl',
// 							'for':'itemInp' + id,
// 							'id':'itemLbl' + id},
// 						   label),
// 				buttons(source, id, "checkbox")
// 	);
// }
//
// function buttons(source, fieldId, type) {
// 	var buttons = [];
// 	var id = "";
//
// 	if(type === "radio") {
// 		id = "itemRbtn" + fieldId;
// 	} else if(type === "checkbox") {
// 		id = "itemChk" + fieldId;
// 	}
// 	var name = id + "options";
//
// 	for(var i=0; i<source.length; i++){
// 		buttons[i] =
// 		$.el.label({'class':type + '-inline'},
// 			$.el.input({'type':type,
// 						'id':id + i,
// 						'value':source[i]}
// 			),
// 			source[i]
// 		);
// 		// Add name attribute if type radio
// 		if(type === "radio")
// 			$(buttons[i]).find("input").attr("name", name);
// 	}
// 	return buttons;
// }
