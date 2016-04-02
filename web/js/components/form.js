/*******************************************************************************
Form
Javascript for adding form elements to pages.
*******************************************************************************/
function Form() {
    this.node = null;

    this.init();
}

Form.prototype.init = function() {
    this.node = this.html();
}

Form.prototype.html = function() {
    return
    $.el.div({'class':'panel panel-default'}, [
        $.el.div({'class':'panel-heading'}),
        $.el.div({'class':'panel-body'})
    ]);
}
