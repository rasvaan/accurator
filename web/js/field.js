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
	});
}

Field.prototype.listen = function() {
	var dropId = '#itemInp' + this.id;

	$(dropId).keyup(function(event) {
		var input = $(dropId).val();

		if(event.which == 13) {
			console.log("SAVE: Enter has been pressed");
		}
		if(event.which == 27) {
			console.log("CANCEL: Esc has been pressed");
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

	// Select the input field and add typeahead
	$(dropId).typeahead({hint: true,
						 highlight: true,
						 minLength: 1},
			   			{name: 'alternatives',
						 source: _field.substringMatcher(array)
	});
}

Field.prototype.substringMatcher = function(strs) {
	return function findMatches(q, cb) {
		var matches, substringRegex;

		// an array that will be populated with substring matches
		matches = [];

		// regex used to determine if a string contains the substring `q`
		substrRegex = new RegExp(q, 'i');

		// iterate through the pool of strings and for any string that
		// contains the substring `q`, add it to the `matches` array
		$.each(strs, function(i, str) {
		  if (substrRegex.test(str)) {
		    matches.push(str);
		  }
		});

		cb(matches);
	}
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
