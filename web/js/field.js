/*******************************************************************************
Field
Class of annotation fields. Fields can be of different types, such as dropdown,
radiobuttons and textfields.
*******************************************************************************/

function Field(type, label, comment, uri, source) {
	this.type = type;
	this.label = label;
	this.comment = comment;
	this.uri = uri;
	this.source = source;
	this.id = generateIdFromUri(uri);
	this.node;
	this.alternatives;
	switch (type) {
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
	var dropId = '#itemInp' + this.id;

	// Eventlistener for selecting typeahead alternative
	$(dropId).on('typeahead:select', function(event, annotationLabel) {
		console.log(event);
		console.log("SAVE: resource EVENT: typeahead:select ANNOTATION: " + annotationLabel);
	});

	// Action upon pressing enter
	$(dropId).on('keyup', function(event) {

		if(event.which == 13) {
			console.log(event);
			var annotationLabel = $(dropId).val();
			console.log("SAVE: literal EVENT: keyup ANNOTATION: " + annotationLabel);
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

	for(var i=0; i<alternatives.results.length; i++)
		array[i] = alternatives.results[i].label;

	// Constructs the suggestion engine
	var bloodHoundAlternatives = new Bloodhound({
		datumTokenizer: Bloodhound.tokenizers.whitespace,
		queryTokenizer: Bloodhound.tokenizers.whitespace,
		local: array
	});

	// Select the input field and add typeahead
	$(dropId).typeahead({hint: true,
						 highlight: true,
						 minLength: 1},
			   			{name: 'alternatives',
						 source: bloodHoundAlternatives,
						 templates: {
							 suggestion: function(data){
								 console.log(data);
								 return '<div>' + data + '</div>';
							 }
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
