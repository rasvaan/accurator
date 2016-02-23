/*******************************************************************************
Annotations
Code for handling the adding and removing of tags, with two main objects:
	* AnnotationList
	* Tag
*******************************************************************************/
function AnnotationList(id) {
	console.log("1.3.2.1 AnnotationList, construct with id: ", id);
	this.id = id; // JQuery Id of the html dom element
	this.divId = "itemDiv" + id; // Id of annotation container
	this.annotations = []; // Array containing the annotations
	this.node = $.el.div({'id':this.divId}) // Html dom element
}

AnnotationList.prototype.add = function(annotation) {
	console.log("1.3.4.1.1 add, add annotation ", annotation);
	// Add annotation to the list
	this.annotations.unshift(annotation);
	console.log("1.3.4.1.2 add, render annotation (should be a listener instead?) ");
	this.render();
}

AnnotationList.prototype.remove = function(annotation) {
	var id = annotation['@id'];

	for (key in this.annotations) {
		var annotationId = this.annotations[key]['@id'];
		// Remove an annotation based on id
		if (annotationId === id)
			this.annotations.splice(key, 1);
	}
	console.log("Removed annotation ", this.annotations);
}

AnnotationList.prototype.render = function() {
	console.log("1.3.4.1.2.1 render, empty the field " + this.id);
	// Empty the field before showing all annotations
	$("#" + this.divId).empty();
	console.log("1.3.4.1.2.2 render, Iterate through the annotations: ", this.annotations);
	// Render the annotations related to this field
	for (var key in this.annotations) {
		var annotation = this.annotations[key];
		var label = truncate(annotation.title, 7);
		var id = generateIdFromUri(annotation['@id']);
		console.log("1.3.4.1.2.2.1 adding annotation with id: ", id);

		// Retrieve target of annotation


		console.log("TARGET: ", this.annotations)
		// Add annotation in div below field
		$('#' + this.divId).append(
			$.el.span({
				'id':'itemLbl' + id,
				'class':'label label-default lblAnnotation'},
				label
			)
		);

		// Add event to label
		$("#itemLbl" + id).on("click", function(){
			console.log("clicked");
		});
	}
}

AnnotationList.prototype.findTarget = function(tag) {
	// Return specific target of a tag or else generic target
	var result = this.findSpecificTarget(tag);

	if (result) {
		return result;
	} else {
		return this.findGenericTarget(tag);
	}
}

AnnotationList.prototype.findSpecificTarget = function(tag) {
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

AnnotationList.prototype.findGenericTarget = function(tag) {
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
