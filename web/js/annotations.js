/*******************************************************************************
Annotations
Code for handling the adding and removing of tags, with two main objects:
	* AnnotationList
	* Tag
*******************************************************************************/
function AnnotationList(id) {
	this.id = id; // Id of the html dom element
	this.annotations = []; // Array containing the annotations
	this.node = $.el.div({'id':id}) // Html dom element
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
	console.log("Removed annotation ", this.annotations);
}

AnnotationList.prototype.render = function() {
	// Empty the field before showing all annotations	
	$('#' + this.id).empty();
	// Render the annotations related to this field
	for (var key in this.annotations) {
		var label = truncate(this.annotations[key].title, 7);
		var id = this.annotations[key].id;

		// Add annotation in div below field
		$('#' + this.id).append(
			$.el.span({
				//TODO: get proper id for annotation
				'id':'itemLbl' + id,
				'class':'label label-default'},
				label
			)
		);

		// Add event to label
		$("#itemLbl" + id).on("click", function(){
			console.log("clicked");
		});
	}
}
