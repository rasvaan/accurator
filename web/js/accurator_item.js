/*******************************************************************************
Accurator Annotate
Code for extending functionality annotation page.
*******************************************************************************/
var query, locale, experiment, domain, user, ui, annotation_ui, uri;
var vntFirstTitle, vntFirstText;
var annotoriousFields;

displayOptions = {
	showMetadata: true,
	showAnnotations: true,
}

function itemInit() {
	locale = getLocale();
	domain = getDomain();
	experiment = getExperiment();
	uri = getParameterByName("uri");

	populateFlags(locale);

	// Make sure user is logged in
	onLoggedIn = function(loginData) {
		setLinkLogo("profile");

		// Get domain settings before populating ui
		onDomain = function(domainData) {
			user = loginData.user;
			var userName = getUserName(loginData.user);
			ui = domainData.ui + "item";
			annotation_ui = domainData.annotation_ui;

			maybeRunExperiment();
			populateUI();
			populateNavbar(userName, [{link:"profile.html", name:"Profile"}]);
		};
		domainSettings = domainSettings(domain, onDomain);
	};
	// If user is not logged go to intro page
	onDismissal = function(){document.location.href="intro.html";};
	logUserIn(onLoggedIn, onDismissal);
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
	.done(function(labels){
		document.title = labels.title;
		initLabels(labels);
		initImage();
		// Only show path when cluster is available TODO: remove ugly check for undefined
		if((localStorage.getItem("currentCluster") !== null) && (localStorage.getItem("currentCluster") !== "undefined") && !(experiment === "random"))
			addPath();
		addButtonEvents();
		events();
	});
	metadata();
	annotations();
}

function initLabels(data) {
	$("#itemBtnPrevious").append(data.itemBtnPrevious);
	$("#itemBtnNext").prepend(data.itemBtnNext);
	$("#navbarBtnRecommend").append(data.navbarBtnRecommend);
	$("#navbarBtnSearch").append(data.navbarBtnSearch);
	vntFirstTitle = data.vntFirstTitle;
	vntFirstText = data.vntFirstText;
	// Add next to optional experiment navigation
	$("#itemBtnExperimentNext").prepend(data.itemBtnNext);
}

function initImage() {
	$.getJSON("metadata", {uri:uri})
	.done(function(metadata){
		// Set id image
		var id = "itemImg" + generateIdFromUri(uri);
		$(".itemImg").attr("id", id);

		// Set location image
		$(".itemImg").attr("src", metadata.image);

		// Add annotation fields for image
		annotationFields(id);
	});
}

function events() {
	$.getJSON("annotations", {uri:user, type:"user"})
	.done(function(annotations){
		uris = annotations;
		if(annotations.length===0) {
			alertMessage(vntFirstTitle, vntFirstText, 'success');
		}
	});
}

function addPath() {
	query = localStorage.getItem("query");
	var cluster = JSON.parse(localStorage.getItem("currentCluster"));
	$("#path").append(pathHtmlElements(cluster.path));
	unfoldPathEvent("#path", cluster.path);
	addNavigationButtonEvents();
}

function addButtonEvents() {
	$("#navbarBtnRecommend").click(function() {
		document.location.href="results.html" + "?user=" + user;
	});
	// Search on pressing enter
	$("#navbarInpSearch").keypress(function(event) {
		if (event.which == 13) {
			var query = encodeURIComponent($("#navbarInpSearch").val());
			document.location.href="results.html?query=" + query;
		}
	});
	$("#navbarBtnSearch").click(function() {
		var query = encodeURIComponent($("#frmSearch").val());
		document.location.href="results.html?query=" + query;
	});
}

function addNavigationButtonEvents() {
	var index = parseInt(localStorage.getItem("itemIndex"));
	var cluster = JSON.parse(localStorage.getItem("currentCluster"));
	var items = cluster.items;

	if(index === 0) {
		$("#itemBtnPrevious").attr("disabled", "disabled");
	} else {
		$("#itemBtnPrevious").click(function() {
			localStorage.setItem("itemIndex", index - 1);
			document.location.href= "annotate.html?uri=" + items[index -1].uri;
		});
	}

	if(index === items.length-1) {
		$("#itemBtnNext").attr("disabled", "disabled");
	} else {
		$("#itemBtnNext").click(function() {
			localStorage.setItem("itemIndex", index + 1);
			document.location.href= "annotate.html?uri=" + items[index + 1].uri;
		});
	}
}

function annotationFields(imageId) {
	// Retrieve the fields that should be added (based on save_user_info)
	$.getJSON("annotation_fields",
			  {locale:locale,
			   domain:domain,
		   	   annotation_ui:annotation_ui})
	.done(function(fields){
		console.log(fields);
		// Add fields whole image
		for(var i=0; i<fields.whole_fields.length; i++) {
			// $("#itemFrmAnnotationFields").append(
			// 	annotationField(fields.whole_fields[i])
			// );
		}
		// Add fields to hidden dom elements for annotorious
		for(var i=0; i<fields.fragment_fields.length; i++) {
			// Create new field object
			var fld = fields.fragment_fields[i];
			var field = new Field(fld.type, fld.label, fld.comment, fld.uri, fld.source);
			$("#itemDivAnnotoriousFields").append(field.node);
			field.listen();
		}
		// annotorious fields are added in deniche init
		anno.addPlugin("DenichePlugin", {});
	});
}

// function annotationField(field) {
//
//
//
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

function addAutocompleteDropdown(id, field) {
	var fieldId = "#itemInp" + id;

	// Get autocomplete alternatives
	var filter = JSON.stringify({scheme:"http://purl.org/vocab/nl/ubvu/BiblePageConceptScheme"});
	var labelRank = "['http://www.w3.org/2004/02/skos/core#prefLabel'-1]";
	$.getJSON("api/autocomplete",
		{q:"pag",
		 filter:filter,
		 labelrank:labelRank,
		 method:"all",
	 	 locale:locale})
	.done(function(alternatives){
		// Add typeahead
		addTypeAhead(fieldId, field, alternatives);
		getInputAnnotationField(fieldId, alternatives);
	});

	// var id = "PageType";
	// var label = "Page type";
	//
	// // Add field
	// $("#itemFrmAnnotationFields").append(annotationField(id, label));
	//
	// // Get autocomplete alternatives
	// var filter = JSON.stringify({scheme:"http://purl.org/vocab/nl/ubvu/BiblePageConceptScheme"});
	// var labelRank = "['http://www.w3.org/2004/02/skos/core#prefLabel'-1]";
	// $.getJSON("api/autocomplete",
	// 	{q:"pag",
	// 	 filter:filter,
	// 	 labelrank:labelRank,
	// 	 method:"all",
	//  	 locale:locale})
	// .done(function(alternatives){
	// 	// Add typeahead
	// 	addTypeAhead(id, alternatives);
	// 	getInputAnnotationField(id, alternatives);
	// 	// Add focus to field
	// 	$("#itemInp" + id).focus();
	// });
}

function addTypeAhead(id, field, alternatives) {
	var array = getAlternativeArray(alternatives.results);

	// Select the input field and add typeahead
	$(field).find(id).typeahead({hint: true,
									  highlight: true,
									  minLength: 1},
									 {name: 'alternatives',
									  source: substringMatcher(array)
	});
	// var array = getAlternativeArray(alternatives.results);
	//
	// $('#itemInp' + id).typeahead({
	// 	hint: true,
	// 	highlight: true,
	// 	minLength: 1
	// },
	// {
	// 	name: 'alternatives',
	// 	source: substringMatcher(array)
	// });
}

function getAlternativeArray(alternatives) {
	var array = [];
	for(var i=0; i<alternatives.length; i++)
		array[i] = alternatives[i].label;
	return array;
}


var substringMatcher = function(strs) {
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

function getInputAnnotationField(id, alternatives) {
	//should allow elements with the same annotation to be added only once
	$('#itemInp' + id).bind('typeahead:select', function(ev, annotationLabel) {
		var annotationUri = "";

		for(var i=0; i<alternatives.results.length; i++) {
			if(annotationLabel.toLowerCase() === alternatives.results[i].label.toLowerCase())
				annotationUri = alternatives.results[i].uri;
		}
		submitAnnotation(uri, annotationUri, annotationLabel, id);
	});
}

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

function submitAnnotation(target, body, label, id, graph) {
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
				$("#itemDivAnnotations").append(
					$.el.span({'id':'itemLblSelected' + id,
							   'class':"label label-danger"},
								label
								//Leave out the remove button for now
								//$.el.span({'class':"glyphicon glyphicon-remove", 'font-size':"1.5em"})
								//'onClick':removeAnnotation($('#itemLblSelected' + id))})
							),
					"&nbsp;"
					);
			}
	});
}

function removeAnotation(annotationLabel) {
	// console.log(annotationLabel);
}

function metadata() {
	if(displayOptions.showMetadata){
		// Get metadata from server
		$.getJSON("metadata", {uri:uri})
		.done(function(metadata){
			appendMetadataWell(metadata);
		});
	}
}

function appendMetadataWell(metadata) {
	$("#itemDivMetadata").append(
		$.el.div({'class':'row'},
			$.el.div({'class':'col-md-10 col-md-offset-1'},
				$.el.div({'class':'well well-sm'},
				  $.el.h4('Showing metadata for ' + metadata.title),
					$.el.dl({'class':'dl-horizontal',
							 'id':'itemLstMetadata'})))));

	for(var i=0; i<metadata.properties.length; i++) {
		var encodedQuery = encodeURIComponent(metadata.properties[i].object_label);
		$("#itemLstMetadata").append(
			$.el.dt(metadata.properties[i].predicate_label));
		$("#itemLstMetadata").append(
			$.el.dd(
				$.el.a({'class':'r_undef',
					    'href':'results.html?query=' + encodedQuery},
					metadata.properties[i].object_label)));
	}
}

function annotations() {
	// Get annotations from server
	if(displayOptions.showAnnotations){
		$.getJSON("annotations", {uri:uri, type:"object"})
		.done(function(annotations){
			if(annotations.annotations.length > 0){
				$("#itemDivMetadata").append(annotationWell(annotations));
			}
		});
	}
}

function annotationWell(annotations) {
	$("#itemDivMetadata").append(
		$.el.div({'class':'row'},
			$.el.div({'class':'col-md-10 col-md-offset-1'},
				$.el.div({'class':'well well-sm'},
				  $.el.h4('Annotations for ' + annotations.title),
					$.el.dl({'class':'dl-horizontal',
							 'id':'itemLstAnnotations'})))));


	for(var i=0; i<annotations.annotations.length; i++) {
		var encodedQuery = encodeURIComponent(annotations.annotations[i].body);
		$("#itemLstAnnotations").append(
			$.el.dt(annotations.annotations[i].field));
		$("#itemLstAnnotations").append(
			$.el.dd(
				$.el.a({'class':'r_undef',
					    'href':'results.html?query=' + encodedQuery},
					annotations.annotations[i].body)));
	}
}

function maybeRunExperiment() {
	// Hide some elements during an experiment
	if(experiment !== "none") {
		// Hide path if on annotate page
		$("#itemDivClusterNavigation").hide();
		// Don't show metadata
		displayOptions.showMetadata = false;
		// Don't show annotations of others
		displayOptions.showAnnotations = false;
		// Add big next button
		addExperimentNavigation();
	}
}

function addExperimentNavigation() {
	$("#itemDivMetadata").before(
		$.el.div({'class':'row',
				  'id':'itemDivNavigationExperiment'},
			$.el.button({'class':'btn btn-primary',
						 'id':'itemBtnExperimentNext'})
		)
	);

	// Get the number of objects annotated
	$.getJSON("annotations", {uri:user, type:"user"})
	.done(function(annotations){
		var numberAnnotated = annotations.length;

		// Switch AB setting after 5 annotations
		if(numberAnnotated == 5) {
			var AorB = getAOrB();

			if(AorB === "recommend")
				setAOrB("random");
			if(AorB === "random")
				setAOrB("recommend")
		}

		// Add click event to navigation button
		$("#itemBtnExperimentNext").click(function() {
			// Go to thank you page after 20 annotations else results
			if(numberAnnotated == 500) {
				document.location.href="end.html";
			} else {
				var items = JSON.parse(localStorage.getItem("currentCluster"));
				var index = items.indexOf(uri);
				var next = index + 1;
				document.location.href="annotate.html" + "?uri=" + items[next];
			}
		});
	});
}
