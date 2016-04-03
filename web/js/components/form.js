/*******************************************************************************
Form
Javascript for adding form elements to pages.
*******************************************************************************/
function Form(id, questions, locale) {
    this.id = id;
    this.questions = questions; // array of questions to ask
    this.locale = locale;
    this.ui = "http://accurator.nl/ui/generic#form";
    this.countries = null; // list of country objects
    this.node = null;


    this.init(questions);
}

Form.prototype.init = function() {
    this.node = this.html();

    for (var i=0; i<this.questions.length; i++) {
        console.log("adding question", i, " with label ", this.questions[i]);
        if (this.questions[i] == "country") this.addCountrySelector();
    }
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
	$(this.node).find("#formLblBirthDate").append(labelData.formLblBirthDate);
	$(this.node).find("#formLblGender").append(labelData.formLblGender);
	$(this.node).find("#formLblCountry").append(labelData.formLblCountry);
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
