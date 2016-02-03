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
	this.node = generateFieldNode();
}

function generateFieldNode() {
	switch (this.type) {
		case "DropdownField":
			return dropdownField();
		case "TextField":
			return textField();
		case "RadioButtonField":
			return radioButtonField();
		case "CheckboxField":
			return checkBoxField();
		case "SelectField":
			return selectField();
	}
}

function dropdownField(id, label, comment) {
	return	$.el.div({'class':'form-group'},
				$.el.label({'class':'itemLbl',
							'for':'itemInp' + id,
							'id':'itemLbl' + id},
						   label),
				$.el.input({'type':'text',
							'class':'form-control typeahead',
							'id':'itemInp' + id,
							'placeholder':comment})
	);
}

function selectField(id, label, comment, source) {
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

function options(source, fieldId) {
	var options = [];
	var id = "itemOpt" + fieldId;

	for(var i=0; i<source.length; i++)
		options[i] = $.el.option(source[i]);
	return options;
}

function textField(id, label, comment) {
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

function radioButtonField(id, label, comment, source) {
	return	$.el.div({'class':'form-group'},
				$.el.label({'class':'itemLbl',
							'for':'itemInp' + id,
							'id':'itemLbl' + id},
						   label),
				buttons(source, id, "radio")
	);
}

function checkBoxField(id, label, comment, source) {
	return	$.el.div({'class':'form-group'},
				$.el.label({'class':'itemLbl',
							'for':'itemInp' + id,
							'id':'itemLbl' + id},
						   label),
				buttons(source, id, "checkbox")
	);
}

function buttons(source, fieldId, type) {
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
