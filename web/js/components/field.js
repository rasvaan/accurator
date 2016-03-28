/*******************************************************************************
Field
Class of annotation fields. Fields are bound to a specific image and annotorious
instance. Most of the code comes from annotate.js written by
Jacco van Ossenbruggen
*******************************************************************************/
function Field(defenition, context) {
	this.id = context.id; // id of field serving as a basis for jquery identifiers
	this.fieldsId = context.fieldsId;  // id of the fields container
	this.inputId = context.id + "Inp"; // id of input
	this.fragmentField = context.fragment; // boolean for indicating this is a fragment field
	this.label = defenition.label; // name of the field
	this.comment = defenition.comment; // short description of the field
	this.node = null; // html node representing the field
	this.source = defenition.source; // source of the alternatives shown
	this.alternatives = null; // list of alternatives for dropdown
	this.showAnnotations = true; // boolean indicating whether previous annotations should be shown
	this.annotationList = null; // array of annotations related to this target and field
	this.field = defenition.uri; // URI identifying annotation field
	this.target = context.target; // URI of target to be annotated
	this.targetImage = context.targetImage; // URI of target's image to be annotated
	this.imageId = context.imageId; // id of the corresponding img element
	this.user = context.user; // URI of the user currently annotating
	this.locale = context.locale // locale for retrieving language specific resources
	this.MOTIVATION = {
		tagging:    'http://www.w3.org/ns/oa#tagging',
		commenting: 'http://www.w3.org/ns/oa#commenting',
		moderating: 'http://www.w3.org/ns/oa#moderating',
	};

	if (this.fragmentField) {
		this._anno = anno; // reference to annotatorious

		// Extend annotatorious with object with fields indexed on imageId and fieldsId
		if (!anno.fields) anno.fields = {};
		if (!anno.fields[this.imageId]) anno.fields[this.imageId] = {};
		var fields = anno.fields[this.imageId][this.fieldsId];
		if (fields) {
			fields.push(this);
		} else {
			anno.fields[this.imageId][this.fieldsId] = [this];
		}
	}
	this.initDropdown();
}

Field.prototype.initDropdown = function() {
	var _field = this; //make sure we can use this Field in $ scope
	this.node = this.dropdownField();

	if (this.showAnnotations) {
		// Add div for annotations, existing annotations are retrieved upon init deniche
		this.annotationList = new AnnotationList(this.id + "DivAnnotations");
		$(this.node).append(this.annotationList.node);
	}

	var addDropdown = function(alternativesArray) {
		_field.addTypeAhead(alternativesArray);
		_field.addDropdownListeners();
	}

	var sourceGet = "all"; //TODO: find good swithc

	// three sitations for obtaining and adding alternatives array
	if(this.source instanceof Array) {
		// 1.  source is an array containing alternatives for dropdown
		console.log("alternatives array ", this.source);
		addDropdown(this.source);
	} else if (sourceGet === "all") {
		console.log("promised all alternatives");
		// 2. all alternatives should be obtained
		this.getAllAlternatives(this.source)
		.then(function(alternativesArray){
			addDropdown(alternativesArray);
		});
	} else if (souceGet === "prefix") {
		console.log("promised all alternatives");
		// 3. event listener should be placed and alternatives are obtained on trigger
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
	if (this.fragmentField && this._anno._deniche.currentShape) {
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

	// console.log("Saving the following ", "field: ", this.field, "hasTarget: ", targetString, "hasBody: ", bodyString, "label: ", label, "motivatedBy: ", motiv, "graph: ", graph);

	$.ajax({type: "POST",
			url: "api/annotation/add",
			data: {
				field: this.field,
				hasTarget: targetString,
				hasBody: bodyString,
				label: label,
			   	motivatedBy: motiv,
				graph: graph
			}})
	.then(function(data) {
		//Add annotation to list of annotations
		_field.annotationList.add(data.annotation);
		_field.addAnnotationFragment(data.annotation, false); // add but do not update open editor
	});
}

Field.prototype.getAnnotations = function() {
	var _field = this; //make sure we can use this Field in $ scope

	$.getJSON("api/annotation/get", {field:this.field, hasTarget:this.target})
	.then(function(data){
		// Get the annotations from the returned data
		var annotations = data[_field.field].annotations;

		for (key in annotations) {
			_field.annotationList.add(annotations[key]);
			_field.addAnnotationFragment(annotations[key], true);
		}
	});
}

Field.prototype.addAnnotationFragment = function(annotation, update) {
	var target = this.annotationList.findSpecificTarget(annotation);
	if (!this.fragmentField || !target) return;

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
	this._anno._deniche.addAnnotation(torious, update);
}

Field.prototype.addDropdownListeners = function() {
	var _field = this; //make sure we can use this Field in $ scope
	var selector = "#" + this.inputId;

	// Eventlistener for selecting typeahead alternative
	$(selector).on('typeahead:select', function(event, annotation) {
		$(selector).typeahead('val', ''); // Clear query

		// console.log("SAVE: resource EVENT: typeahead:select ANNOTATION: ", annotation);
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

			// console.log("SAVE: literal EVENT: keyup ANNOTATION: ", annotation);
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

	// Action on pressing esc
	$(selector).on('keyup', function(event) {
		if (_field.fragmentField && event.which == 27) {
			_field._anno._deniche.onFragmentCancel(event);
		}
	});
}

Field.prototype.getAlternatives = function(defenition) {
	// Get autocomplete alternatives
	var filter = JSON.stringify({scheme:"http://accurator.nl/bible#BiblicalFigureConceptScheme"});
	var labelRank = "['http://www.w3.org/2004/02/skos/core#prefLabel'-1]";

	// Return promise
	return $.getJSON("api/autocomplete",
		{q:string,
		 filter:filter,
		 labelrank:labelRank,
		//  method:"all",
		 locale:this.locale});
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
		 locale:this.locale});
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
	return	$.el.div({'class':'form-group'},
				$.el.label({'class':'itemLbl',
							'for':this.inputId},
						   this.label),
				$.el.input({'type':'text',
							'class':'form-control typeahead',
							'id':this.inputId,
							'placeholder':this.comment}));
}
