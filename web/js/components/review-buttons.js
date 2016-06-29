/*******************************************************************************
Review buttons
*******************************************************************************/
function ReviewButtons(annotationUri, parentId, size) {
    this.annotationUri = annotationUri; // annotation to be reviewed
    this.fieldName = "http://accurator.nl/ui/annotation/review#Buttons";
    this.size = size ? "btn-" + size : "";
    this.parentId = parentId;
    this.node = null;

    this.init();
}

ReviewButtons.prototype.init = function() {
    this.node =
        $.el.div({'class':'btn-group'},
            $.el.button({'class':'btn btnReview btn-success ' + this.size},
                $.el.span({'class':'glyphicon glyphicon-thumbs-up'}),
                    " agree"
            ),
            $.el.button({'class':'btn btnReview btn-warning ' + this.size},
                $.el.span({'class':'glyphicon glyphicon-thumbs-down'}),
                    "  disagree"
            ),
            $.el.button({'class':'btn btnReview btn-danger ' + this.size},
                $.el.span({'class':'glyphicon glyphicon-remove'}),
                    " remove"
            )
        );
    this.buttonEvents();
}

ReviewButtons.prototype.buttonEvents = function() {
    var _buttons = this;
    $(this.node).find(".btn-success").on("click", function() {
        _buttons.submitReview(_buttons.annotationUri, {'@value':"agree"})
        .then(function(){
            _buttons.paintParent("green");
        }, function() {
            alert("Could not save review");
        });
    });
    $(this.node).find(".btn-warning").on("click", function() {
        _buttons.submitReview(_buttons.annotationUri, {'@value':"disagree"})
        .then(function(){
            _buttons.paintParent("orange");
        }, function() {
            alert("Could not save review");
        });
    });
    $(this.node).find(".btn-danger").on("click", function() {
        _buttons.deleteAnnotation(_buttons.annotationUri, "Removed from dashboard application")
        .then(function(){
            _buttons.paintParent("red");
        }, function() {
            alert("Could not delete annotation");
        });
    });
}

ReviewButtons.prototype.paintParent = function(color) {
    // remove other colors
    $("#" + this.parentId).removeClass("success warning danger");

	if (color === "green") {
        $("#" + this.parentId).addClass('success');
	} else if (color === "orange") {
		$("#" + this.parentId).addClass('warning');
	} else if (color === "red") {
		$("#" + this.parentId).addClass('danger');
	}
}

ReviewButtons.prototype.submitReview = function(target, body, label, graph) {
	if (!target) return; // review target is required
	if (!body) return; // review in the form of text or resource is required
    if (!label && body['@value']) label = body['@value']; // set label to value in body if not sepperately defined
	if (!graph)	graph = target;

	return $.ajax({
        type: "POST",
        url: "/api/annotation/add",
		data: {
		    field: this.fieldName,
			hasTarget: JSON.stringify([{'@id':target}]),
			hasBody: JSON.stringify(body),
			label: label,
			motivatedBy: 'http://www.w3.org/ns/oa#moderating',
			graph: graph
	}});
}

ReviewButtons.prototype.deleteAnnotation = function(annotationUri, comment) {
	if (!annotationUri) return; // annotation to be deleted is required
	if (!comment) comment = "Removed in during review process";

    return $.ajax({
        type: "DELETE",
        url: "/api/annotation/remove?" +
            "annotation=" + encodeURIComponent(annotationUri) +
            "&comment=" + encodeURIComponent(comment)
	});
}
