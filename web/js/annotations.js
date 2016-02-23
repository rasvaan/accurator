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
		var label = truncate(this.annotations[key].title, 7);
		var id = generateIdFromUri(this.annotations[key]['@id']);
		console.log("1.3.4.1.2.2.1 adding annotation with id: ", id);
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
