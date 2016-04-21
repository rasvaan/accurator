/* Annotorious plugin to use the annotation editor from the
 * ClioPatria image_annotation cpack package as the editor for
 * shapes created in Annotorious.
 *
 * To this, we need to solve three issues:
 *
 * 1. We need to communicate the shape that the user created within
 *    annotorious to all annotation field objects of the cpack.
 *    This is why the plugin has a public member variable 'currentShape'
 *    that the cpack fields can access.
 *
 * 2. The cpack data model assumes multiple annotation can be made on
 *    the same target, while annotorious can have only one.
 *    As a result, we need to collect all tags for the same target and
 *    merge them into a single annotation for annotorious.
 *    This is done in the 'cleantags' array, which is indexed on targetId.
 *
 * 3. While the annotorious editor is open, the user can add and remove
 *    individual tags using the cpack user interface. But we can only
 *    add/rm the final merged tag using the annotorious API when the
 *    editor is closed. As a result, we need to do some bookkeeping
 *    ourselves in the _dirtytag variable, and update annotorious when
 *    the editor is closed by flushing out all changes made to the current
 *    dirty tag.
 *
 * Apart from these issues above, the approach is quite straight forward:
 *
 * 1. We simple move the cpack editor as a plugin field into the annotorious
 *    editor.
 * 2. We hijack the cancel/save buttons of annotorious for our own purposes.
 * 3. We bind handler functions on all relevant events generated by annotorious.
 *
 * @author: Jacco van Ossenbruggen
 *
 * */

annotorious.plugin.DenichePlugin = function(labels) {
	/** @public **/
	this.currentShape = null; // Should be accessible by cpack objects

	/** @private **/
	this._cleantags = []; // tags annotorious already knows about
	this._dirtytag = null; // tag annotorious doesn't know yet
	this._labels = labels; // labels for buttons
}

annotorious.plugin.DenichePlugin.prototype.onInitAnnotator = function(annotator) {
    this.annotator = annotator;
	var el =  annotator.element;

	// remove standard textarea
	var textArea = el.getElementsByTagName('textarea')[0];
	textArea.parentNode.removeChild(textArea);

	// move the cpack editor into the annotorious editor:
    var fieldsId = el.getElementsByTagName('img')[0].getAttribute('fields');
    var imageId  = el.getElementsByTagName('img')[0].getAttribute('id');
    var fieldsEl = document.getElementById(fieldsId);
    annotator.editor.addField(fieldsEl);

	// get the annotorious save and cancel button so we can manipulate them:
	var node = $(this.annotator.element);
	var saveButton = node.find(".annotorious-editor-button-save");
	var cancelButton = node.find(".annotorious-editor-button-cancel");
	// change labels and change styling
	saveButton.html(this._labels.annoBtnDone);
	cancelButton.html(this._labels.annoBtnCancel);
	saveButton.addClass("btn btn-primary btn-sm");
	cancelButton.addClass("btn btn-default btn-sm");

    // install all handlers on events created by annotorious:
    this.installHandlers();

	// get existing annotations on init annotorious
    if (this._anno.fields) {
		var fields = this._anno.fields[imageId][fieldsId];

		for (var i in fields) {
			var field = fields[i];

			if (field.showAnnotations) field.getAnnotations();
		}
    }
}

annotorious.plugin.DenichePlugin.prototype.initPlugin = function(anno) {
	this._anno = anno;	// make sure we know annotorious ...
	this._anno._deniche = this; // and annotorious knows us
}

annotorious.plugin.DenichePlugin.prototype.filterTags = function(targetId, fieldsId) {
	// filter tags to show only the ones with the same selector
	var oSelf = this;
	var editor = $(".annotorious-editor")[0];
	var selector = '#'+ fieldsId + ' .lblAnnotation';
	if (!fieldsId) selector = '.lblAnnotation';

	$(editor).find(selector).each(function(index, annotation) {
		// see if id matches the (current?) target
		if (targetId == $(annotation).attr("targetId")) {
			$(annotation).show();
		} else {
			$(annotation).hide();
		}
	});
}

annotorious.plugin.DenichePlugin.prototype.removeAnnotation = function (label, targetId) {
	var old = this._dirtytag;
	if (!old) old = this._cleantags[targetId];
	if (old) {
		var annotation = JSON.parse(JSON.stringify(old)); // deep copy
		var index = old.compound_text.indexOf(label);
		annotation.compound_text = old.compound_text.splice(index, 1);
		annotation.text = old.compound_text.join(', ');
		this._dirtytag = annotation;
	}
}

annotorious.plugin.DenichePlugin.prototype.onFragmentCancel = function(ev) {
	this.annotator.stopSelection();
	this.annotator.editor.close();
}

annotorious.plugin.DenichePlugin.prototype.addAnnotation = function (annotation, update) {
	// possibly get old tag
	var old = this._dirtytag;
	if (!old) old = this._cleantags[ annotation.targetId ];

	if (old) {
		// extend new annotation by merging in old one
		annotation.compound_text = old.compound_text;
		annotation.compound_text.push(annotation.text);
		annotation.text = annotation.compound_text.join(', ');
	} else {
		annotation.compound_text = [ annotation.text ];
	}

	// if update is true or instantiated add to the rendered tags
	if (update) {
		this._cleantags[annotation.targetId] = annotation;
		this._anno.addAnnotation(annotation, old);
	} else {
		this._dirtytag = annotation;
	}

	this.filterTags(annotation.targetId, annotation.fieldsId); // only show tags for this shape
}

annotorious.plugin.DenichePlugin.prototype.flushDirtyAnnotation = function(original) {
	var dirty = this._dirtytag;
	this._dirtytag = null; // set to null since it will be processed

	if (dirty) {
		if (dirty.text) {
			this._anno.addAnnotation(dirty,original);
			this._cleantags[dirty.targetId] = dirty;
		} else {
			this._anno.removeAnnotation(original);
			this._cleantags[dirty.targetId] = null;
		}
	}
}

annotorious.plugin.DenichePlugin.prototype.installHandlers = function() {
	var oSelf = this;
	var node = $(oSelf.annotator.element);

	this._anno.addHandler('onSelectionCompleted', function(event) {
		oSelf.currentShape = event.shape;
 	});

	this._anno.addHandler('onEditorShown', function(annotation) {
		// set focus on first field (exlude hint input field introduced by twitter typeahead)
		node.find(".annotorious-editor input").not(".tt-hint")[0].focus();

		oSelf._dirtytag = null;
		if (annotation.text && annotation.shapes) {
			oSelf.currentShape = annotation.shapes[0];
			oSelf.filterTags(annotation.targetId, annotation.fieldsId); // only show tags for this shape
		} else {
			oSelf.filterTags(null, null);	// hide all tags
		}
	});

	this._anno.addHandler('onAnnotationCreated', function(original) {
		// triggered when done is clicked
		oSelf.flushFields(original);
	});

	this._anno.addHandler('onAnnotationUpdated', function(original) {
		// triggered when done is clicked and there already was an annotation
		oSelf.flushFields(original);
	});
}

annotorious.plugin.DenichePlugin.prototype.flushFields = function(original) {
	var oSelf = this;
	var node = $(oSelf.annotator.element);
	var fieldsId = this.annotator.element.getElementsByTagName('img')[0].getAttribute('fields');
	var imageId  = this.annotator.element.getElementsByTagName('img')[0].getAttribute('id');
	var fields = this._anno.fields[imageId][fieldsId];
	var counter = 0;
	var promises =[];

	// itterate through fields, saving not entered values
	for (var i=0; i<fields.length; i++) {
		var inputField = $(fields[i].node).find("#" + fields[i].inputId);

		if (inputField.val()) {
			var annotation = inputField.val();

			// if there is a value, create promise to enable waiting for it to be saved
			promises[counter] = fields[i].submitAnnotation(
				fields[i].MOTIVATION.tagging,
				fields[i].target,
				{'@value':annotation},
				annotation
			);

			inputField.typeahead('val', ''); // clear input
			counter++;
		}
	}

	if (promises.length > 0) {
		// wait for all the annotatoins to be added before flushing
		$.when.apply($, promises)
		.then(function() {
			oSelf.flushDirtyAnnotation(original);
		});
	} else {
		// no promises to wait for
		oSelf.flushDirtyAnnotation(original);
	}
}
