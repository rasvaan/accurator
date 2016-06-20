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
			// another annotation on selector with existing id (target id is the selector, not the image)
			targetObject = [{hasSource:targetImage, hasSelector:{value:shape}}, {'@id':target}];
		} else {
			// annotation on new selector, id will be generated server-side
			targetObject = [{hasSource:target, hasSelector:{value:shape}}];
		}
	} else {
		// annotation without fragment, on entire target image
		targetObject = [{'@id':target}];
	}

	var targetString = JSON.stringify(targetObject);
	var bodyString = JSON.stringify(body);

	return $.ajax({type: "POST",
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
		// add annotation to list of annotations
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

/*******************************************************************************
TextField
*******************************************************************************/
function TextField (field, context) {
	Field.call(this, field, context);
	this.node = this.html();

	if (this.showAnnotations) {
		// add div for annotations, existing annotations are retrieved upon init deniche
		this.annotationList = new AnnotationList(this.id + "DivAnnotations");
		$(this.node).append(this.annotationList.node);
	}

	this.addEventListeners();
}

TextField.prototype = Object.create(Field.prototype); // inherit

TextField.prototype.html = function() {
	return	$.el.div({'class':'form-group'},
				$.el.label({'class':'itemLbl',
							'for':this.inputId},
						   this.label),
				$.el.input({'type':'text',
							'class':'form-control',
							'id':this.inputId,
							'placeholder':this.comment,
							'autocomplete':'off'})
	);
}

TextField.prototype.addEventListeners = function() {
	var _field = this; //make sure we can use this Field in $ scope
	var inputField = $(this.node).find("#" + this.inputId); // reach the input

	// prevent standard form submit
	inputField.keydown(function(event){
		if(event.keyCode == 13) {
			event.preventDefault();
			return false;
		}
	});

	// action upon pressing enter
	inputField.on('keyup', function(event) {
		// check to see if there is a value on pressing enter
		if (inputField.val() && event.which == 13) {
			var annotation = inputField.val();

			// console.log("SAVE: literal EVENT: keyup ANNOTATION: ", annotation);
			_field.submitAnnotation(
				_field.MOTIVATION.tagging,
				_field.target,
				{'@value':annotation},
				annotation
			);

			// clear input
			inputField.val('');
		}
	});

	// action on pressing esc
	inputField.on('keyup', function(event) {
		if (_field.fragmentField && event.which == 27) {
			_field._anno._deniche.onFragmentCancel(event);
		}
	});
}

/*******************************************************************************
DropdownField
*******************************************************************************/
function DropdownField (field, context) {
	Field.call(this, field, context);
	this.source = field.source; // source of the alternatives shown
	this.alternatives = null; // list of alternatives for dropdown
	this.node = this.html();

	if (this.showAnnotations) {
		// add div for annotations, existing annotations are retrieved upon init deniche
		this.annotationList = new AnnotationList(this.id + "DivAnnotations");
		$(this.node).append(this.annotationList.node);
	}

	this.initAlternatives();
}

DropdownField.prototype = Object.create(Field.prototype); // inherit

DropdownField.prototype.html = function() {
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

DropdownField.prototype.initAlternatives = function() {
	var _field = this; //make sure we can use this Field in $ scope

	// three sitations for obtaining and adding alternatives array
	if (this.source instanceof Array) {
		// 1.  source is an array containing alternatives for dropdown
		this.alternatives = this.source;
		this.addTypeAhead();
		this.addDropdownListeners();
	} else if (this.source.api === "/api/autocomplete/all") {
		// 2. all alternatives should be obtained
		this.getAllAlternatives()
		.then(function(alternativesArray){
			_field.alternatives = alternativesArray;
			_field.addTypeAhead();
			_field.addDropdownListeners();
		});
	} else if (this.source.api === "/api/autocomplete") {
		// 3. event listener should be placed and alternatives are obtained on trigger
		// this.addTypeAhead();
		this.addDropdownListeners();
	}
}


DropdownField.prototype.addDropdownListeners = function() {
	var _field = this; //make sure we can use this Field in $ scope
	var inputField = $(this.node).find("#" + this.inputId); // reach the input

	// eventlistener for selecting typeahead alternative
	inputField.on('typeahead:select', function(event, annotation) {
		inputField.typeahead('val', ''); // Clear query

		// console.log("SAVE: resource EVENT: typeahead:select ANNOTATION: ", annotation);
		_field.submitAnnotation(
			_field.MOTIVATION.tagging,
			_field.target,
			{'@id':annotation.uri},
			annotation.value
		);
	});

	// action upon pressing enter
	inputField.on('keyup', function(event) {
		// Check to see if typeahead cleared the field (so autocomplete was used)
		if (inputField.val() && event.which == 13) {
			var annotation = inputField.val();

			// console.log("SAVE: literal EVENT: keyup ANNOTATION: ", annotation);
			_field.submitAnnotation(
				_field.MOTIVATION.tagging,
				_field.target,
				{'@value':annotation},
				annotation
			);
			// Clear input
			inputField.typeahead('val', '');
		}
	});

	// action on pressing esc
	inputField.on('keyup', function(event) {
		if (_field.fragmentField && event.which == 27) {
			_field._anno._deniche.onFragmentCancel(event);
		}
	});
}

DropdownField.prototype.getAllAlternatives = function() {
	//HACK for getting EN resources Iconclass
	var locale = this.locale;
	if (this.source.filterScheme === "http://accurator.nl/bible#BiblicalThemeConceptScheme") {
		locale = "en";
	}

	// Get autocomplete alternatives
	var filter = JSON.stringify({scheme: this.source.filterScheme});
	// var labelRank = "['http://www.w3.org/2004/02/skos/core#prefLabel'-1]";

	// Return promise
	return $.getJSON("api/autocomplete", {
		q:"stub",
		filter:filter,
		// labelrank:labelRank,
		method:"all",
		locale:locale //HACK: should be this.locale
	});
}

DropdownField.prototype.addTypeAhead = function() {
	var bloodHound = this.createBloodhound(); // constructs the suggestion engine
	var suggestionTemplate = this.createSuggestionTemplate();

	// add typeahead
	$(this.node).find("#" + this.inputId).typeahead({
		hint: true,
		highlight: true,
		minLength: 1
	}, {
		name:'alternatives',
		display:'value',
		source: bloodHound,
		templates: {
			suggestion: suggestionTemplate
		}
	});
}

DropdownField.prototype.createBloodhound = function() {
	var array = [];

	if (this.alternatives == null) {
		// did not obtain alternatives from source or all, so should get them on the fly
		return null;
	} else if (this.source instanceof Array) {
		// list of labels from source (different array, since we have no uri)
		for(var i=0; i<this.alternatives.length; i++)
			array[i] = {value: this.alternatives[i]};

		return new Bloodhound({
			datumTokenizer: Bloodhound.tokenizers.obj.whitespace('value'),
			queryTokenizer: Bloodhound.tokenizers.whitespace,
			local: array // add data
		});
	} else if (this.source.api === "/api/autocomplete/all"){
		// prep the data for adding it to the suggestion engine
		for(var i=0; i<this.alternatives.results.length; i++) {
			array[i] = {
				value:this.alternatives.results[i].label,
				uri:this.alternatives.results[i].uri
			};
		}

		return new Bloodhound({
			datumTokenizer: Bloodhound.tokenizers.obj.whitespace('value'),
			queryTokenizer: Bloodhound.tokenizers.whitespace,
			local: array // add data
		});
	}
}

DropdownField.prototype.createSuggestionTemplate = function() {
	if (this.source instanceof Array) {
		// simple layout for array source
		return suggestionTemplate = function(data) {
			return '<div>' + data.value + '</div>';
		}
	} else {
		// otherwise we should have the uri available
		return suggestionTemplate = function(data) {
			return '<div>' + data.value + '</div>';
			//return '<div>' + data.value + ' - <small>' + data.uri + '</small></div>';
		}
	}
}
