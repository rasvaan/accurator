/*******************************************************************************
Form
Javascript for adding form elements to pages.
*******************************************************************************/
function Form(questions) {
    this.questions = questions; // array of questions to ask
    this.node = null;

    this.init(questions);
}

Form.prototype.init = function() {
    this.node = this.html();

    for (var i=0; i<this.questions.length; i++) {
        console.log("adding question", i, " with label ", this.questions[i]);
        if (this.questions[i] == "country") this.addCountryQuestion();
    }
}

Form.prototype.html = function() {
    return $.el.div({'class':'panel panel-default'}, [
        $.el.div({'class':'panel-heading'}),
        $.el.div({'class':'panel-body'})
    ]);
}

Form.prototype.addCountryQuestion = function() {
    console.log("should be adding country question");
    $(this.node).find(".panel-body").append("country");
}
