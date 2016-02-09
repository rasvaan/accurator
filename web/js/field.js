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

	switch (defenition.type) {
		case "DropdownField":
			this.initDropdown();
			break;
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
}

Field.prototype.addDropdownListeners = function() {
	var _field = this; //make sure we can use this Field in $ scope
	var dropId = '#itemInp' + this.id;

	// Eventlistener for selecting typeahead alternative
	$(dropId).on('typeahead:select', function(event, annotation) {
		console.log("SAVE: resource EVENT: typeahead:select ANNOTATION: ", annotation);
		// Submit the resource
		_field.submitAnnotation(target, body, label, id);
		// Code for clearing query
		//$('input.typeahead').typeahead('setQuery', '');
	});

	// Action upon pressing enter
	$(dropId).on('keyup', function(event) {

		if(event.which == 13) {
			console.log(event);
			var annotation = $(dropId).val();
			console.log("SAVE: literal EVENT: keyup ANNOTATION: ", annotation);
		}
	});
}

Field.prototype.submitAnnotation = function(target, body, label, id, graph) {
	console.log("Should be submitting", target, body, label, id, graph);
	if (!graph)
		graph = target;

	var targetJson = JSON.stringify([{'@id':target}]);
	var bodyJson = JSON.stringify({'@id':body});
	var field = "page type";

	$.ajax({type: "POST",
			url: "api/annotation/add",
			data: {hasTarget:targetJson,
				   hasBody:bodyJson,
			   	   graph:graph,
			   	   field:field},
			success: function(){
				//Add label indicating in the UI what has been added
				// $("#itemDivAnnotations").append(
				// 	$.el.span({'id':'itemLblSelected' + id,
				// 			   'class':"label label-danger"},
				// 				label
				// 				//Leave out the remove button for now
				// 				//$.el.span({'class':"glyphicon glyphicon-remove", 'font-size':"1.5em"})
				// 				//'onClick':removeAnnotation($('#itemLblSelected' + id))})
				// 			),
				// 	"&nbsp;"
				// 	);
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
	return	$.el.div({'class':'form-group'},
				$.el.label({'class':'itemLbl',
							'for':'itemInp' + this.id,
							'id':'itemLbl' + this.id},
						   this.label),
				$.el.input({'type':'text',
							'class':'form-control typeahead',
							'id':'itemInp' + this.id,
							'placeholder':this.comment})
	);
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
