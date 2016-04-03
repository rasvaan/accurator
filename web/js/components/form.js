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
    this.removeFormEvents();
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

Form.prototype.removeFormEvents = function() {
    // make sure no weird form events are triggered on keypress
    $(this.node).on('keyup keypress', function(e) {
        var keyCode = e.keyCode || e.which;

        if (keyCode === 13) {
            e.preventDefault();
            return false;
        }
    });
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

        if (this.groupIds[i] == "gender") {
            var buttons = this.genderButtons(labelData);

            this.formGroups[i] = new RadioFormGroup(
                this.groupIds[i],
                labelData.formLblGender,
                buttons
            );
        }

        if (this.groupIds[i] == "socialNetwork") {
            var boxes = this.socialBoxes(labelData);

            this.formGroups[i] = new SocialCheckBoxFormGroup(
                this.groupIds[i],
                labelData.formLblSocialNetwork,
                boxes
            );
        }
    }

    // add form groups to html node
    for (var i=0; i<this.formGroups.length; i++) {
        $(this.node).find(".form-horizontal").append(
            this.formGroups[i].node
        );
    }

	$(this.node).find("#formLblMail").append(labelData.formLblMail);
	$(this.node).find("#formLblEmailCheck").append(labelData.formLblEmailCheck);
	$(this.node).find("#formLblTaggingExperience").append(labelData.formLblTaggingExperience);
	$(this.node).find("#formLblTagSite").append(labelData.formLblTagSite);
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

    return array;
}

Form.prototype.genderButtons = function(labels) {
    return buttons = [
        {'id':'male', 'label':labels.formRbtnMale},
        {'id':'female', 'label':labels.formRbtnFemale}
    ];
}

Form.prototype.socialBoxes = function(labels) {
    return boxes = [
        {'id':'facebook', 'label':labels.formChkFacebook},
        {'id':'linkedIn', 'label':labels.formChkLinkedIn},
        {'id':'twitter', 'label':labels.formChkTwitter},
        {'id':'none', 'label':labels.formChkOther},
        {'id':'none', 'label':labels.formChkNone}
    ];
}

Form.prototype.processFormFields = function() {
    var info = {};

    for (var i=0; i<this.formGroups.length; i++) {
        var id = this.formGroups[i].id;
        var value = this.formGroups[i].getValue();

        if (value instanceof Object) {
            $.extend(info, value); // add object values to info
        } else if (!(value == null)) {
            info[id] = value; // add single value to info
        }
    }
    console.log("info result ", info);
	return save_user_info(info);
}

/*******************************************************************************
FormGroup
*******************************************************************************/

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

/*******************************************************************************
TextFormGroup
*******************************************************************************/

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

/*******************************************************************************
RadioFormGroup
*******************************************************************************/

function RadioFormGroup(id, label, buttons) {
    FormGroup.call(this, id, label);
    this.buttons = buttons; // array with objects with labels and ids for buttons
    this.addRadioButtons();
}

RadioFormGroup.prototype = Object.create(FormGroup.prototype); // inherit

RadioFormGroup.prototype.addRadioButtons = function() {
    $(this.node).find("div").append(
        this.buttonNodes()
    );
}

RadioFormGroup.prototype.buttonNodes  = function() {
    var buttonNodes = [];

    for (var i=0; i<this.buttons.length; i++) {
        buttonNodes[i] = $.el.label({'class':'radio-inline'}, [
            $.el.input({
                'type':'radio',
                'name':'formRbtns' + this.id,
                'id':'formRbtn' + this.buttons[i].id,
                'value':this.buttons[i].id
            }),
            $.el.span(this.buttons[i].label)
        ])
    }

    return buttonNodes;
}

RadioFormGroup.prototype.getValue = function() {
    var value = $("input[name='formRbtns" + this.id + "']:checked").val();

    if (value === undefined) {
        return null;
    } else {
		return value;
    }
}

/*******************************************************************************
CheckBoxFormGroup
*******************************************************************************/

function CheckBoxFormGroup(id, label, boxes) {
    FormGroup.call(this, id, label);
    this.boxes = boxes; // array with objects with labels and ids for buttons
    this.addCheckBoxes();
}

CheckBoxFormGroup.prototype = Object.create(FormGroup.prototype); // inherit

CheckBoxFormGroup.prototype.addCheckBoxes = function() {
    $(this.node).find("div").append(
        this.boxNodes()
    );
}

CheckBoxFormGroup.prototype.boxNodes  = function() {
    var boxNodes = [];

    for (var i=0; i<this.boxes.length; i++) {
        boxNodes[i] = $.el.label({'class':'checkbox-inline'}, [
            $.el.input({
                'type':'checkbox',
                'id':'formChk' + this.boxes[i].id,
                'value':this.boxes[i].id
            }),
            $.el.span(this.boxes[i].label)
        ])
    }

    return boxNodes;
}

function SocialCheckBoxFormGroup(id, label, boxes) {
    CheckBoxFormGroup.call(this, id, label, boxes);
}

SocialCheckBoxFormGroup.prototype = Object.create(CheckBoxFormGroup.prototype);

SocialCheckBoxFormGroup.prototype.getValue = function() {
    var info = {};

    if($("#formChknone").is(":checked")) {
        info.facebook = false;
        info.linked_in = false;
        info.twitter = false;
    } else {
        if ($("#formChkfacebook").is(":checked"))
            info.facebook = true;
        if ($("#formChklinkedIn").is(":checked"))
            info.linked_in = true;
        //Set twitter to true when no id is given but box is checked
        if ($("#formChktwitter").is(":checked")) {
            if (!($("#addTwitterId").val() === undefined) && !($("#addTwitterId").val() === "")) {
                info.twitter = $("#addTwitterId").val();
            } else {
                info.twitter = true;
            }
        }
        //Set social to true
        if ($("#formChkother").is(":checked")) {
            if (!($("#addSocialSite").val() === undefined) && !($("#addSocialSite").val() === "")) {
                info.other_social_site = $("#addSocialSite").val();
            } else {
                info.other_social_site = true;
            }
        }
    }
    console.log("info in getValue ", info)
    return info;
    // var value = $("input[name='formRbtns" + this.id + "']:checked").val();
    //
    // if (value === undefined) {
    //     return null;
    // } else {
	// 	return value;
    // }
}

/*******************************************************************************
SelectFormGroup
*******************************************************************************/

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
