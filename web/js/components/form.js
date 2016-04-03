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
    		twitterFieldAdded: false,
    		tagsiteFieldAdded: false,
    		socialFieldAdded: false,
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
            this.formGroups[i].addAlternatives("countries", this.locale, this.ui);
            console.log("adding question", i, " with object ", this.formGroups[i]);
        }
    }

    // add form groups to html node
    for (var i=0; i<this.formGroups.length; i++) {
        $(this.node).find(".form-horizontal").append(
            this.formGroups[i].node
        );
    }
    // $(this.node).find("#formLblCountry").append(labelData.formLblCountry);


	$(this.node).find("#formLblBirthDate").append(labelData.formLblBirthDate);
	$(this.node).find("#formLblGender").append(labelData.formLblGender);

	$(this.node).find("#formLblCommunity").append(labelData.formLblCommunity);
	$(this.node).find("#formLblLanguage").append(labelData.formLblLanguage);
	$(this.node).find("#formLblEducation").append(labelData.formLblEducation);
	$(this.node).find("#formLblMail").append(labelData.formLblMail);
	$(this.node).find("#formLblEmailCheck").append(labelData.formLblEmailCheck);
	$(this.node).find("#formLblSocialNetwork").append(labelData.formLblSocialNetwork);
	$(this.node).find("#formLblInternetUsage").append(labelData.formLblInternetUsage);
	$(this.node).find("#formLblMuseumVisits").append(labelData.formLblMuseumVisits);
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
            $(_form.node).hide();
        });
    });

    $(this.node).find("#formBtnSkip").click(function() {
        $(_form.node).hide();
    });
}

Form.prototype.processFormFields = function() {
    var info = {};
    console.log("process form fields");
    // this.getInputDropdownMenus();

	return save_user_info(info);
}

Form.prototype.addCountrySelector = function() {
    $(this.node).find("form").append(
        $.el.div({'class':'form-group'}, [
            $.el.label({
                'class':'col-sm-5 control-label',
                'for':'formSltCountry',
                'id':'formLblCountry'
            }),
            $.el.div({'class':'col-sm-5'},
                $.el.select({
                    'class':'form-control',
                    'id':'formSltCountry'
                })
            )
        ])
    );
    this.addCountries();
}

Form.prototype.addCountries = function() {
    $.getJSON("ui_elements", {
        locale: this.locale,
        ui: this.ui,
        type:"countries"
    })
    .then(function(countryData) {
        this.countries = countryData;
        this.countries.sort(function(a, b) {
            return a.name.localeCompare(b.name)
        });

        // add first empty element
        $("#formSltCountry").append($.el.option(""));

        // add countries to selector
        for (var i = 0; i < this.countries.length; i++) {
            $("#formSltCountry").append($.el.option(this.countries[i].name));
        }
    }, function(){
        $("#formSltCountry").append($.el.option("No countries found on server"));
    });
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

SelectFormGroup.prototype.addAlternatives = function(type, locale, ui) {
    var _group = this;

    $.getJSON("ui_elements", {
        locale: locale,
        ui: ui,
        type: type
    })
    .then(function(alternativeData) {
        _group.alternatives = alternativeData;
        _group.alternatives.sort(function(a, b) {
            return a.name.localeCompare(b.name)
        });

        // add first empty element
        $(_group.node).find("#formSlt" + _group.id).append($.el.option(""));

        // add countries to selector
        for (var i = 0; i < _group.alternatives.length; i++) {
            $(_group.node).find("#formSlt" + _group.id).append(
                $.el.option(_group.alternatives[i].name)
            );
        }
    }, function(){
        $(_group.node).find("#formSlt" + _group.id).append(
            $.el.option("No values found on server")
        );
    });
}
