/*******************************************************************************
Form
Javascript for adding form elements to pages.
*******************************************************************************/
function Form(id, groupIds, locale) {
    this.id = id;
    this.groupIds = groupIds; // array of questions to ask
    this.locale = locale;
    this.ui = "http://accurator.nl/ui/generic#form";
    this.formGroups = [];
    this.countries = null; // list of country objects
    this.node = null;
    this.twitterFieldAdded = false;
    this.tagsiteFieldAdded = false;
    this.socialFieldAdded = false;

    this.init();
}

Form.prototype.init = function() {
    this.node = this.html();
}

Form.prototype.html = function() {
    return $.el.div({'class':'panel panel-primary', 'id':this.id}, [
        $.el.div({'class':'panel-heading', 'id':'formHdr'}),
        $.el.div({'class':'panel-body'},
            $.el.form({'class':'form-horizontal'})
        ),
        $.el.div({'class':'panel-footer'}, [
            $.el.button({'class':'btn btn-primary', 'id':'formBtnAdd'}),
            $.el.button({'class':'btn btn-link', 'id':'formBtnSkip'})
        ])
    ]);
}

Form.prototype.addText = function() {
    // Just try adding the complete bunch and see what works
    var _form = this;

    return getLabels(this.locale, this.ui).then(function(labelData) {
        var labels = {
    		formLblTwitterId: labelData.formLblTwitterId,
    		formLblTagSiteOpen: labelData.formLblTagSiteOpen,
    		formLblSocialSiteOpen: labelData.formLblSocialSiteOpen,
    		formOptsEducation: labelData.formOptsEducation,
    		formOptsInternet: labelData.formOptsInternet,
    		formTxtDisclaimer: labelData.formTxtDisclaimer,
    		formHdrDisclaimer: labelData.formHdrDisclaimer
    	};

        _form.addHeader(labelData);
        _form.addFormQuestions(labelData);
        _form.addButtons(labelData)
    });
}

Form.prototype.addHeader = function(labelData) {
    if (this.id === "formInternet") {
        $(this.node).find("#formHdr").append(
            labelData.formHdrInternetUsage
        );
    }

    if (this.id === "formPersonal") {
        $(this.node).find("#formHdr").append(
            labelData.formHdrPersonalInfo
        );
    }
}

Form.prototype.addFormQuestions = function(labelData) {
    // create from group objects based on labels and ids
    for (var i=0; i<this.groupIds.length; i++) {
        if (this.groupIds[i] == "country") {
            this.formGroups[i] = new SelectFormGroup(
                this.groupIds[i],
                labelData.formLblCountry
            );
            this.formGroups[i].getAlternatives("countries", this.locale, this.ui);
        }

        if (this.groupIds[i] == "language") {
            this.formGroups[i] = new SelectFormGroup(
                this.groupIds[i],
                labelData.formLblLanguage
            );
            this.formGroups[i].getAlternatives("languages", this.locale, this.ui);
        }

        if (this.groupIds[i] == "education") {
            this.formGroups[i] = new SelectFormGroup(
                this.groupIds[i],
                labelData.formLblEducation
            );
            this.formGroups[i].alternatives = this.educationAlternatives(labelData);
            this.formGroups[i].addAlternatives();
        }

        if (this.groupIds[i] == "birthDate") {
            this.formGroups[i] = new TextFormGroup(
                this.groupIds[i],
                labelData.formLblBirthDate
            );
        }
    }

    // add form groups to html node
    for (var i=0; i<this.formGroups.length; i++) {
        $(this.node).find(".form-horizontal").append(
            this.formGroups[i].node
        );
    }

	$(this.node).find("#formLblBirthDate").append(labelData.formLblBirthDate);
	$(this.node).find("#formLblGender").append(labelData.formLblGender);
	$(this.node).find("#formLblMail").append(labelData.formLblMail);
	$(this.node).find("#formLblEmailCheck").append(labelData.formLblEmailCheck);
	$(this.node).find("#formLblSocialNetwork").append(labelData.formLblSocialNetwork);
	$(this.node).find("#formLblInternetUsage").append(labelData.formLblInternetUsage);
	$(this.node).find("#formLblMuseumVisits").append(labelData.formLblMuseumVisits);
	$(this.node).find("#formLblTaggingExperience").append(labelData.formLblTaggingExperience);
	$(this.node).find("#formLblTagSite").append(labelData.formLblTagSite);
}

Form.prototype.educationAlternatives = function(labels) {
    var array = [
        labels.formOptsEducation.formOptPrimarySchool,
        labels.formOptsEducation.formOptHighSchool,
        labels.formOptsEducation.formOptCollege,
        labels.formOptsEducation.formOptBachelor,
        labels.formOptsEducation.formOptMaster,
        labels.formOptsEducation.formOptDoctorate,
        labels.formOptsEducation.formOptUnkown
    ];

    console.log(array);
    return array;
}

Form.prototype.addButtons = function(labelData) {
    $(this.node).find("#formBtnAdd").append(labelData.formBtnAdd);
    $(this.node).find("#formBtnSkip").append(labelData.formBtnSkip);
    this.addButtonEvents();
}

Form.prototype.addButtonEvents = function() {
    var _form = this;

    $(this.node).find("#formBtnAdd").click(function() {
        _form.processFormFields()
        .then(function() {
            // $(_form.node).hide();
        });
    });

    $(this.node).find("#formBtnSkip").click(function() {
        $(_form.node).hide();
    });
}

Form.prototype.processFormFields = function() {
    var info = {};

    for (var i=0; i<this.formGroups.length; i++) {
        var id = this.formGroups[i].id;
        var value = this.formGroups[i].getValue();

        if (!(value == null)) info[id] = value;
    }
    console.log("info ", info);
	return save_user_info(info);
}

function FormGroup(id, label) {
    this.id = id;
    this.label = label;
    this.node = null;

    this.init();
}

FormGroup.prototype.init = function() {
    this.node = $.el.div({'class':'form-group', 'id':'formDiv' + this.id}, [
        $.el.label({'class':'col-sm-5 control-label', 'id':'formLbl' + this.id},
            this.label),
        $.el.div({'class':'col-sm-5'})
    ]);
}

function TextFormGroup(id, label) {
    FormGroup.call(this, id, label);

    this.addTextField();
}

TextFormGroup.prototype = Object.create(FormGroup.prototype); // inherit

TextFormGroup.prototype.addTextField = function() {
    // change the class
    $(this.node).find("div").addClass('col-sm-2');
    $(this.node).find("div").removeClass('col-sm-5');

    $(this.node).find("label").attr("for", "formInp" + this.id);

    $(this.node).find("div").append(
        $.el.input({
            'class':'form-control',
            'id':'formInp' + this.id,
            'type':'date'
        })
    );
}

TextFormGroup.prototype.getValue = function() {
    var input = $(this.node).find("#formInp" + this.id).val();

    if (!(input === "")) {
        return input;
    } else {
        return null;
    }
}

function SelectFormGroup(id, label) {
    FormGroup.call(this, id, label);
    this.alternatives = null;

    this.addSelect();
}

SelectFormGroup.prototype = Object.create(FormGroup.prototype); // inherit

SelectFormGroup.prototype.addSelect = function() {
    $(this.node).find("label").attr("for", "formSlt" + this.id);

    $(this.node).find("div").append(
        $.el.select({'class':'form-control', 'id':'formSlt' + this.id})
    );
}

SelectFormGroup.prototype.getAlternatives = function(type, locale, ui) {
    var _group = this;

    $.getJSON("ui_elements", {
        locale: locale,
        ui: ui,
        type: type
    })
    .then(function(alternativeData) {
        _group.alternatives = alternativeData;
        _group.alternatives.sort(function(a, b) {
            return a.label.localeCompare(b.label)
        });

        _group.addAlternatives();
    }, function(){
        $(_group.node).find("#formSlt" + _group.id).append(
            $.el.option("No values found on server")
        );
    });
}

SelectFormGroup.prototype.addAlternatives = function(alternatives) {
    // add first empty element
    $(this.node).find("#formSlt" + this.id).append($.el.option(""));

    // add alternatives to selector
    for (var i = 0; i < this.alternatives.length; i++) {
        $(this.node).find("#formSlt" + this.id).append(
            $.el.option(this.alternatives[i].label)
        );
    }
}

SelectFormGroup.prototype.getValue = function() {
    var input = $(this.node).find("#formSlt" + this.id).val();

    // Find the id corresponding to the selected name
    for (var key in this.alternatives) {
        if (this.alternatives[key].label === input) {
            return this.alternatives[key].id;
        }
    }
    return null;
}
