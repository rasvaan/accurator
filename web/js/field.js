/*******************************************************************************
Field
Class of annotation fields. Fields can be of different types, such as dropdown,
radiobuttons and textfields. Part of the code comes from annotate.js written by
Jacco van Ossenbruggen
*******************************************************************************/

function Field(defenition, target, targetImage, user) {
	this.field = defenition.uri; // URI identifying annotation field
	this.type = defenition.type; // type of input field (e.g. dropdown or radiobutton)
	this.label = defenition.label; // name of the field
	this.comment = defenition.comment; // short description of the field
	this.source = defenition.source; // source of the alternatives shown
	this.id = generateIdFromUri(defenition.uri); // id of the field useable by jquery
	this.node = null; // html node representing the field
	this.alternatives = null; // list of alternatives for dropdown
	this.target = target; // URI of target to be annotated
	this.targetImage = null; // URI of target's image to be annotated
	this.user = user; // URI of the user currently annotating
	this._anno = anno; // Jacco hack to get to annotourious
	this.annotations = []; // Array of annotations related to this target and field
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
			success: function(){
				//Add annotation to list of annotations
				_field.addAnnotation(label);
				_field.renderAnnotations();
			}
	});
}

Field.prototype.getAnnotationsPromise = function() {
	console.log("get annotations");
	// Return promise
	return $.getJSON("api/annotation/get",
		{field:this.field,
		hasTarget:this.target});
}

Field.prototype.addAnnotation = function(label, id) {
	console.log("Adding annotation to arrary: ", label, id);
	// Add annotation to list of annotations
	// For know annotation is a string (boring)
	this.annotations.unshift({label:label, id:id});
}

Field.prototype.renderAnnotations = function() {
	// Render the annotations related to this field
	for (var key in this.annotations) {
		var label = truncate(this.annotations[key].label, 7);
		var id = this.annotations[key].id;

		// Add annotation in div below field
		$("#itemDiv" + this.id + "Annotations").append(
			$.el.span({
				//TODO: get proper id for annotation
				'id':'itemLbl' + id,
				'class':'label label-default'},
				label
			)
		);

		// Add event to label
		$("#itemLbl" + id).on("click", function(){
			console.log("clicked");
		});
	}
}

Field.prototype.initDropdown = function() {
	var _field = this; //make sure we can use this Field in $ scope

	this.node = this.dropdownField();
	this.getAllAlternatives()
	.then(function(alternatives){
		_field.addTypeAhead(alternatives);
		_field.addDropdownListeners();
	});

	this.getAnnotationsPromise()
	.then(function(data){
		// Get the annotations from the returned data
		var annotations = data[_field.field].annotations;
		for (key in annotations) {
			var label = annotations[key].title;
			var id = generateIdFromUri(annotations[key]['@id']);
			_field.addAnnotation(label, id);
		}
		_field.renderAnnotations();
	});
}

Field.prototype.addDropdownListeners = function() {
	var _field = this; //make sure we can use this Field in $ scope
	var dropId = '#itemInp' + this.id;

	// Eventlistener for selecting typeahead alternative
	$(dropId).on('typeahead:select', function(event, annotation) {
		console.log("SAVE: resource EVENT: typeahead:select ANNOTATION: ", annotation);
		_field.submitAnnotation(
			_field.MOTIVATION.tagging,
			_field.target,
			{'@id':annotation.uri},
			annotation.value
		);
		// Code for clearing query
		//$('input.typeahead').typeahead('setQuery', '');
	});

	// Action upon pressing enter
	$(dropId).on('keyup', function(event) {
		if(event.which == 13) {
			console.log(event);
			var annotation = $(dropId).val();

			console.log("SAVE: literal EVENT: keyup ANNOTATION: ", annotation);
			_field.submitAnnotation(
				_field.MOTIVATION.tagging,
				_field.target,
				{'@value':annotation},
				annotation
			);
			// Clear input
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
	return	[$.el.div({'class':'form-group'},
				$.el.label({'class':'itemLbl',
							'for':'itemInp' + this.id,
							'id':'itemLbl' + this.id},
						   this.label),
				$.el.input({'type':'text',
							'class':'form-control typeahead',
							'id':'itemInp' + this.id,
							'placeholder':this.comment})),
			$.el.div({'id':'itemDiv' + this.id + 'Annotations'})];
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

Field.prototype.findTarget = function(tag) {
	//TODO: might be moved in to Tag object
	// Return specific target of a tag or else generic target
	var result = this.findSpecificTarget(tag);

	if (result) {
		return result;
	} else {
		return this.findGenericTarget(tag);
	}
}

Field.prototype.findGenericTarget = function(tag) {
	//TODO: might be moved in to Tag object
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
