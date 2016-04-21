/*******************************************************************************
Annotations
Code for handling the adding and removing of tags
*******************************************************************************/
function AnnotationList(id) {
	this.divId = id; // id of annotation container
	this.annotations = []; // array containing the annotations
	this.node = $.el.div({'id':id}) // empty html dom element
}

AnnotationList.prototype.add = function(annotation) {
	// Add annotation to the list
	this.annotations.unshift(annotation);
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
}

AnnotationList.prototype.render = function() {
	var node = $(this.node);

	// empty the field before showing all annotations
	node.empty();

	// Render the annotations related to this field
	for (var key in this.annotations) {
		var annotation = this.annotations[key];
		var label = truncate(annotation.title, 10);
		var id = generateIdFromUri(annotation['@id']);
		var target = this.findTarget(annotation);

		// Add annotation in div below field
		node.append(
			$.el.span({
				'id':'lbl' + id,
				'class':'label label-primary lblAnnotation',
				'targetId':target['@id']},
				label
			)
		);

		// Add event to label
		// $("#lbl" + id).on("click", function(){
		// });
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


	if (!targets)
		return null; // return null if no target is known
	if (targets['@id'])
		return targets; // return targets array if key @id is present

	// Loop through targets till key @id is found
	for (var t in targets) {
		target = targets[t];
		if (target['@id'])
			return target;
	}
	return null;
}
