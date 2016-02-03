/*******************************************************************************
Field
Class of annotation fields. Fields can be of different types, such as dropdown,
radiobuttons and textfields.
*******************************************************************************/

function Field(type, label, comment, uri) {
	this.type = type;
	this.label = label;
	this.comment = comment;
	this.uri = uri;
	this.id = generateIdFromUri(uri);
	this.node = this.generateFieldNode();
}

Field.prototype.listen = function() {
	console.log("Should be broadcasting");
	var dropId = '#itemInp' + this.id;
	$(dropId).keyup(function(event) {
		console.log($(dropId).val());
		if(event.which == 13) {
			console.log("SAVE: Enter has been pressed");
		}
		if(event.which == 27) {
			console.log("CANCEL: Esc has been pressed");
		}
	});
}

Field.prototype.generateFieldNode = function() {
	console.log(this.type);
	switch (this.type) {
		case "DropdownField":
			return this.dropdownField();
		case "TextField":
			return this.textField();
		case "RadioButtonField":
			return this.radioButtonField();
		case "CheckboxField":
			return this.checkBoxField();
		case "SelectField":
			return this.selectField();
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

Field.prototype.selectField = function() {
	return	$.el.div({'class':'form-group'},
				$.el.label({'class':'itemLbl',
							'for':'itemInp' + id,
							'id':'itemLbl' + id},
						   label),
				$.el.select({'class':'form-control',
							 'id':'itemSlt' + id,
							 'placeholder':comment},
						 	 options(source, id))
	);
}

Field.prototype.options = function() {
	var options = [];
	var id = "itemOpt" + fieldId;

	for(var i=0; i<source.length; i++)
		options[i] = $.el.option(source[i]);
	return options;
}

Field.prototype.textField = function() {
	return	$.el.div({'class':'form-group'},
				$.el.label({'class':'itemLbl',
							'for':'itemInp' + id,
							'id':'itemLbl' + id},
						   label),
				$.el.textarea({'type':'text',
							   'class':'form-control',
							   'id':'itemInp' + id,
							   'rows':'2',
							   'placeholder':comment})
	);
}

Field.prototype.radioButtonField  = function() {
	return	$.el.div({'class':'form-group'},
				$.el.label({'class':'itemLbl',
							'for':'itemInp' + id,
							'id':'itemLbl' + id},
						   label),
				buttons(source, id, "radio")
	);
}

Field.prototype.checkBoxField = function() {
	return	$.el.div({'class':'form-group'},
				$.el.label({'class':'itemLbl',
							'for':'itemInp' + id,
							'id':'itemLbl' + id},
						   label),
				buttons(source, id, "checkbox")
	);
}

Field.prototype.buttons = function() {
	var buttons = [];
	var id = "";

	if(type === "radio") {
		id = "itemRbtn" + fieldId;
	} else if(type === "checkbox") {
		id = "itemChk" + fieldId;
	}
	var name = id + "options";

	for(var i=0; i<source.length; i++){
		buttons[i] =
		$.el.label({'class':type + '-inline'},
			$.el.input({'type':type,
						'id':id + i,
						'value':source[i]}
			),
			source[i]
		);
		// Add name attribute if type radio
		if(type === "radio")
			$(buttons[i]).find("input").attr("name", name);
	}
	return buttons;
}
