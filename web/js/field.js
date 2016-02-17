/*******************************************************************************
Field
Class of annotation fields. Fields are bound to a specific image and annotorious
instance. Most of the code comes from annotate.js written by
Jacco van Ossenbruggen
*******************************************************************************/
function Field(defenition, context) {
	this.id = context.id; // id of the field useable by jquery
	this.imageId = context.imageId; // Id of the corresponding img element
	this.fieldsId = context.fieldsId;
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
	this.annotationList = new AnnotationList('itemDiv' + _field.id + 'Annotations');
	this.node = this.dropdownField();
	// Get already existing annotations for field
	if(this.showAnnotations) this.getAnnotations();

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
		console.log("Got the follpwing annotations: ", data);
		// Get the annotations from the returned data
		var annotations = data[_field.field].annotations;
		var length = annotations.length;

		for (key in annotations) {
			_field.annotationList.add(annotations[key]);
			_field.addAnnotationFragment(annotations[key], true);
		}
	});
}

Field.prototype.addAnnotationFragment = function(annotation, update) {
	var target = this.findSpecificTarget(annotation);
	if (!this._anno || !target) return;

	var label = annotation.title;
	var x = target.hasSelector.x;
	var y = target.hasSelector.y;
	var w = target.hasSelector.w;
	var h = target.hasSelector.h;
	var torious = {
		src: $("#" + this.imageId).attr("src"),
		text: label,
		targetId: target['@id'],
		fieldsId: this.fieldsId,
		annotationId: annotation.annotation,
		shapes: [{
			type:'rect',
			geometry: { x:x,y:y,width:w,height:h }
		}]
	};
	this._anno._deniche.addAnnotation(torious, update);
}

Field.prototype.findSpecificTarget = function(tag) {
	// Returns the specific fragmet target of a tag identified by a selector
	var targets = tag.hasTarget;
	var target = undefined;

	if (!targets)
		return null;
	if (targets.hasSelector)
		return targets;
	for (var t in targets) {
		target = targets[t];
		if (target.hasSelector)
			return target;
	}
	return null;
}

Field.prototype.findTarget = function(tag) {
	// Return specific target of a tag or else generic target
	var result = this.findSpecificTarget(tag);

	if (result) {
		return result;
	} else {
		return this.findGenericTarget(tag);
	}
}

Field.prototype.findGenericTarget = function(tag) {
	// Returns the generic target of a tag identified with key @id
	var targets = tag.hasTarget;
	var target = undefined;

	// Return null if no target is known
	if (!targets)
		return null;
	// Return targets array if key @id is present
	if (targets['@id'])
		return targets;
	// Loop through targets till key @id is found
	for (var t in targets) {
		target = targets[t];
		if (target['@id'])
			return target;
	}
	return null;
}
Field.prototype.addDropdownListeners = function() {
	var _field = this; //make sure we can use this Field in $ scope
	var dropId = '#itemInp' + this.id;

	// Eventlistener for selecting typeahead alternative
	$(dropId).on('typeahead:select', function(event, annotation) {
		// Code for clearing query
		$(dropId).typeahead('val', '');

		console.log("SAVE: resource EVENT: typeahead:select ANNOTATION: ", annotation);
		_field.submitAnnotation(
			_field.MOTIVATION.tagging,
			_field.target,
			{'@id':annotation.uri},
			annotation.value
		);
	});

	// Action upon pressing enter
	$(dropId).on('keyup', function(event) {
		// Check to see if typeahead cleared the field (so autocomplete was used)
		if ($(dropId).val() && event.which == 13) {
			var annotation = $(dropId).val();

			console.log("SAVE: literal EVENT: keyup ANNOTATION: ", annotation);
			_field.submitAnnotation(
				_field.MOTIVATION.tagging,
				_field.target,
				{'@value':annotation},
				annotation
			);
			// Clear input
			$(dropId).typeahead('val', '');
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
	var dropId = '#itemInp' + this.id;
	var _field = this; //make sure we can use this Field in $ scope
	var array = [];

	// Prep the data for adding it to the suggestion engine
	for(var i=0; i<alternatives.results.length; i++) {
		array[i] = {
			value:alternatives.results[i].label,
			uri:alternatives.results[i].uri
		};
	}

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

	// Select the input field and add typeahead
	$(dropId).typeahead({hint: true,
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
	// Return the form group and a list for the annotations
	return	$.el.div({'class':'form-group', 'id':'itemDiv' + this.id},
				$.el.label({'class':'itemLbl',
							'for':'itemInp' + this.id,
							'id':'itemLbl' + this.id},
						   this.label),
				$.el.input({'type':'text',
							'class':'form-control typeahead',
							'id':'itemInp' + this.id,
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
