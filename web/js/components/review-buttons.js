/*******************************************************************************
Review buttons
*******************************************************************************/
function ReviewButtons(annotationUri, size) {
    this.annotationUri = annotationUri; // annotation to be reviewed
    this.size = size ? "btn-" + size : "";
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
        alert("you successfully clicked a button for " + _buttons.annotationUri);
    });
    $(this.node).find(".btn-warning").on("click", function() {
        alert("you successfully clicked an orange button");
    });
    $(this.node).find(".btn-danger").on("click", function() {
        alert("you successfully clicked a dangerous button");
    });
}
