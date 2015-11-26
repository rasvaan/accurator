/*******************************************************************************
Accurator Form
Code for rendering fields used for elliciting information about user.
*******************************************************************************/
var locale, domain, experiment, ui;
var countries = [];
var languages = [];
var formOptsEducation,  formOptsInternet;
var info = {};
var twitterFieldAdded = false;
var tagsiteFieldAdded = false;
var socialFieldAdded = false;
var formLblTwitterId, formLblTagSiteOpen, formLblSocialSiteOpen;
var formTxtDisclaimer, formHdrDisclaimer;

function formInit() {
	locale = getLocale();
	domain = getDomain();
	experiment = getExperiment();
	populateFlags(locale);

	// Make sure user is logged in
	onLoggedIn = function(loginData){
		setLinkLogo("profile");

		//Get domain settings before populating ui
		onDomain = function(domainData) {
			ui = domainData.ui + "form";
			populateUI();
			var userName = getUserName(loginData.user);
			populateNavbar(userName, [{link:"profile.html", name:"Profile"}]);
		};
		domainSettings = domainSettings(domain, onDomain);
	};
	onDismissal = function(){document.location.href="intro.html"};
	logUserIn(onLoggedIn, onDismissal);
}

function nextPage() {
	// Determine which page will be shown next
	if(experiment === "true") {
		return function(){document.location.href="expertise.html"};
	} else {
		return function(){document.location.href="domain.html"};
	}
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
	.done(function(data){
		addButtonEvents();
		initLabels(data);
		addFormEvents();
		alertMessage(formHdrDisclaimer, formTxtDisclaimer, 'info');
	});
}

function addButtonEvents() {
	$("#formBtnAdd").click(function() {
		processFormFields();
	});
	$("#formBtnSkip").click(function() {
		// Get function for next page and execute
		nextPage()();
	});
}

function initLabels(labels) {
	document.title = labels.formPageTitle;
	$("#formHdrInternetUsage").append(labels.formHdrInternetUsage);
	$("#formHdrPersonalInfo").append(labels.formHdrPersonalInfo);
	formLblTwitterId = labels.formLblTwitterId;
	formLblTagSiteOpen = labels.formLblTagSiteOpen;
	formLblSocialSiteOpen = labels.formLblSocialSiteOpen;
	$("#formHdrSlogan").prepend(labels.formHdrSlogan);
	$("#formHdrSubSlogan").append(labels.formHdrSubSlogan);
	initFormQuestions(labels);
	$("#formBtnAdd").append(labels.formBtnAdd);
	$("#formBtnSkip").append(labels.formBtnSkip);
	initRadioButtons(labels);
	initCheckboxes(labels);
	initCountriesSelector();
	initLanguagesSelector();
	formOptsEducation = labels.formOptsEducation;
	initEducationSelector();
	formOptsInternet = labels.formOptsInternet;
	initInternetSelector(labels.formOptsInternet);
	formTxtDisclaimer = labels.formTxtDisclaimer;
	formHdrDisclaimer = labels.formHdrDisclaimer;
}

function initFormQuestions(data) {
	$("#formLblBirthDate").append(data.formLblBirthDate);
	$("#formLblGender").append(data.formLblGender);
	$("#formLblCountry").append(data.formLblCountry);
	$("#formLblCommunity").append(data.formLblCommunity);
	$("#formLblLanguage").append(data.formLblLanguage);
	$("#formLblEducation").append(data.formLblEducation);
	$("#formLblMail").append(data.formLblMail);
	$("#formLblEmailCheck").append(data.formLblEmailCheck);
	$("#formLblSocialNetwork").append(data.formLblSocialNetwork);
	$("#formLblInternetUsage").append(data.formLblInternetUsage);
	$("#formLblMuseumVisits").append(data.formLblMuseumVisits);
	$("#formLblTaggingExperience").append(data.formLblTaggingExperience);
	$("#formLblTagSite").append(data.formLblTagSite);
}

function initRadioButtons(data) {
	$("#formRbtnMale").after(data.formRbtnMale);
	$("#formRbtnFemale").after(data.formRbtnFemale);
	$("#formRbtnUrban").after(data.formRbtnUrban);
	$("#formRbtnSubUrban").after(data.formRbtnSubUrban);
	$("#formRbtnRural").after(data.formRbtnRural);
	$("#formRbtnNone").after(data.formRbtnNone);
	$("#formRbtnNovice").after(data.formRbtnNovice);
	$("#formRbtnIntermediate").after(data.formRbtnIntermediate);
	$("#formRbtnExpert").after(data.formRbtnExpert);
}

function initCheckboxes(data) {
	$("#formChkFacebook").after(data.formChkFacebook);
	$("#formChkLinkedIn").after(data.formChkLinkedIn);
	$("#formChkTwitter").after(data.formChkTwitter);
	$("#formChkOther").after(data.formChkOther);
	$("#formChkNone").after(data.formChkNone);
	$("#formChkTagFlickr").after(data.formChkTagFlickr);
	$("#formChkTagDelicious").after(data.formChkTagDelicious);
	$("#formChkTagFacebook").after(data.formChkTagFacebook);
	$("#formChkTagOther").after(data.formChkTagOther);
	$("#formChkTagNone").after(data.formChkTagNone);
}

function addFormEvents() {
	$("#formChkOther").click(function() {
		if(!socialFieldAdded) {
			$("#formDivSocialNetwork").after(
				$.el.div({'class':'form-group'},
						$.el.label({'for':'addSocialSite',
									'id':'frmSocialOpen',
									'class':'col-sm-5 control-label'},
									formLblSocialSiteOpen),
						$.el.div({'class':'col-sm-5'},
								 $.el.input({'type':'text',
									 		 'id':'addSocialSite',
									 		 'class':'form-control'}))));
			socialFieldAdded = true;
		}
	});
	$("#formChkTwitter").click(function() {
		if(!twitterFieldAdded) {
			$("#formDivSocialNetwork").after(
				$.el.div({'class':'form-group'},
						$.el.label({'for':'addTwitterId',
									'id':'formLblTwitterId',
									'class':'col-sm-5 control-label'},
									formLblTwitterId),
						$.el.div({'class':'col-sm-5'},
								 $.el.input({'type':'text',
									 		 'id':'addTwitterId',
									 		 'class':'form-control'}))));
			twitterFieldAdded = true;
		}
	});
	$("#formChkTagOther").click(function() {
		if(!tagsiteFieldAdded) {
			$("#formDivTaggingSite").after(
				$.el.div({'class':'form-group'},
					$.el.label({'for':'addTagSite',
							    'id':'formLblTagSite',
								'class':'col-sm-5 control-label'},
								formLblTagSiteOpen),
					$.el.div({'class':'col-sm-5'},
							 $.el.input({'type':'text',
										 'id':'addTagSite',
										 'class':'form-control'}))));
			tagsiteFieldAdded = true;
		}
	});
}

function initCountriesSelector() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"countries"})
	.done(function(data){
		for (var key in data) {
			countries[key] = {"name":data[key].name, "country_code":data[key].country_code};
		}
		countries.sort(function(a,b) { return a.name.localeCompare(b.name) });

		$("#formSltCountry").append($.el.option(""));
		for (var i=0; i<countries.length; i++) {
			$("#formSltCountry").append($.el.option(countries[i].name));
		}
	})
	.fail(function(data, textStatus){
		$("#formSltLanguage").append($.el.option("No countries found on server"));
	});
}

function initLanguagesSelector() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"languages"})
	.done(function(data){
		for (var key in data) {
			languages[key] = {"iso_code":data[key].iso_code, "name":data[key].name};
		}
		languages.sort(function(a,b) { return a.name.localeCompare(b.name) });

		$("#formSltLanguage").append($.el.option(""));
		for (var i=0; i<languages.length; i++) {
			$("#formSltLanguage").append($.el.option(languages[i].name));
		}
	})
	.fail(function(data, textStatus){
		$("#formSltLanguage").append($.el.option("No languages found on server"));
	});
}

function initEducationSelector() {
	$("#formSltEducation").append($.el.option(""));
	$("#formSltEducation").append($.el.option(formOptsEducation.formOptPrimarySchool.label));
	$("#formSltEducation").append($.el.option(formOptsEducation.formOptHighSchool.label));
	$("#formSltEducation").append($.el.option(formOptsEducation.formOptCollege.label));
	$("#formSltEducation").append($.el.option(formOptsEducation.formOptBachelor.label));
	$("#formSltEducation").append($.el.option(formOptsEducation.formOptMaster.label));
	$("#formSltEducation").append($.el.option(formOptsEducation.formOptDoctorate.label));
	$("#formSltEducation").append($.el.option(formOptsEducation.formOptUnkown.label));
}

function initInternetSelector(optionList) {
	$("#formSltInternet").append($.el.option(""));
	$("#formSltInternet").append($.el.option(formOptsInternet.formOptInternetAlways.label));
	$("#formSltInternet").append($.el.option(formOptsInternet.formOptInternetOnceADay.label));
	$("#formSltInternet").append($.el.option(formOptsInternet.formOptInternet3to5.label));
	$("#formSltInternet").append($.el.option(formOptsInternet.formOptInternet1to2.label));
	$("#formSltInternet").append($.el.option(formOptsInternet.formOptInternetLessThan1.label));
}

function processFormFields() {
	getInput();
	var onSuccess = nextPage();
	save_user_info(info, onSuccess);
}

function getInput() {
	getInputTextFields();
	getInputRadioButtons();
	getInputCheckboxes();
	getInputDropdownMenus();
}

function getInputTextFields() {
	if (!($("#formInpAddAge").val() === ""))
		info.age = $("#formInpAddAge").val();
	if (!($("#formInpAddMail").val() === ""))
		info.mail = $("#formInpAddMail").val();
	if (!($("#formInpAddMuseumVisits").val() === ""))
		info.museum_visits = $("#formInpAddMuseumVisits").val();
}

function getInputRadioButtons() {
	if(!($("input[name='formRbtnsGender']:checked").val() === undefined))
		info.gender = $("input[name='formRbtnsGender']:checked").val();
	if(!($("input[name='communityRadio']:checked").val() === undefined))
		info.community = $("input[name='communityRadio']:checked").val();
	if(!($("input[name='formRbtnsTaggingExperience']:checked").val() === undefined))
		info.tagging_experience_level = $("input[name='formRbtnsTaggingExperience']:checked").val();
}

function getInputCheckboxes() {
	getInputSocialNetwork();
	getInputTaggingSite();
	getInputEmailCheck();
}

function getInputSocialNetwork() {
	if($("#formChkNone").is(":checked")) {
		info.facebook = false;
		info.linked_in = false;
		info.twitter = false;
	} else {
		if ($("#formChkFacebook").is(":checked"))
			info.facebook = true;
		if ($("#formChkLinkedIn").is(":checked"))
			info.linked_in = true;
		//Set twitter to true when no id is given but box is checked
		if ($("#formChkTwitter").is(":checked")) {
			if (!($("#addTwitterId").val() === undefined) && !($("#addTwitterId").val() === "")) {
				info.twitter = $("#addTwitterId").val();
			} else {
				info.twitter = true;
			}
		}
		//Set social to true
		if ($("#formChkOther").is(":checked")) {
			if (!($("#addSocialSite").val() === undefined) && !($("#addSocialSite").val() === "")) {
				info.other_social_site = $("#addSocialSite").val();
			} else {
				info.other_social_site = true;
			}
		}
	}
}

function getInputTaggingSite() {
	if($("#formChkTagNone").is(":checked")) {
		info.flickr = false;
		info.delicious = false;
		info.tag_facebook = false;
		info.other = false;
	} else {
		if ($("#formChkTagFlickr").is(":checked"))
			info.flickr = true;
		if ($("#formChkTagDelicious").is(":checked"))
			info.delicious = true;
		if ($("#formChkTagFacebook").is(":checked"))
			info.tag_facebook = true;

		//Set tagsite to true
		if ($("#formChkTagOther").is(":checked")) {
			if (!($("#addTagSite").val() === undefined) && !($("#addTagSite").val() === "")) {
				info.other_tag_site = $("#addTagSite").val();
			} else {
				info.other_tag_site = true;
			}
		}
	}
}

function getInputEmailCheck() {
	if($("#formChkEmail").is(":checked")) {
		info.accurator_email = true;
	} else {
		info.accurator_email = false;
	}
}

function getInputDropdownMenus() {
	if (!($("#formSltCountry").val() === ""))
		info.country = getCountryId($("#formSltCountry").val());
	if (!($("#formSltLanguage").val() === ""))
		info.language = getLanguageCode($("#formSltLanguage").val());
	if (!($("#formSltEducation").val() === ""))
		info.education = getOptionId(formOptsEducation, $("#formSltEducation").val());
	if (!($("#formSltInternet").val() === ""))
		info.internet_use = getOptionId(formOptsInternet, $("#formSltInternet").val());
}

function getCountryId(name) {
	// Find the geonames id corresponding to the selected name
	for(var i=0; i<countries.length; i++) {
		if (countries[i].name === name)
			return countries[i].country_code;
	}
}

function getLanguageCode(name) {
	// Find the iso code corresponding to the selected name
	for(var i=0; i<languages.length; i++) {
		if (languages[i].name === name)
			return languages[i].iso_code;
	}
}

function getOptionId(optionList, name) {
	// Find the id corresponding to the selected name
	for (var key in optionList) {
		if(optionList[key].label === name)
			return optionList[key].id;
	}
}
