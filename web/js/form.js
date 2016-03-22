/*******************************************************************************
Accurator Form

Code for rendering fields used for elliciting information about user.
*******************************************************************************/
"use strict";

// Initialize page
function formInit() {
	var locale = getLocale();
	var domain = getDomain();

	populateFlags(locale);

	userLoggedIn()
	.then(function(userData){
		//user is logged in, draw page
		drawPage(userData);
	}, function() {
		// user is not logged in, show modal
		var onDismissal = function() {
			document.location.href = "intro.html";
		};

		login(drawPage, onDismissal);
	});

	function drawPage(userData){
		var user = userData.user;
		var userName = getUserName(user);

		var ui, labels;
		var countries = [];
		var languages = [];
		var info = {};

		setLinkLogo("profile");
		populateNavbar(userName, [{link:"profile.html",	name:"Profile"}], locale);

		domainSettings(domain)
		.then(function (domainData) {
			ui = domainData.ui + "form";

			return getLabels(locale, ui);
		})
		.then(function(labelData) {
			labels = initLabels(locale, ui, countries, languages, labelData);
			addButtonEvents(info, labels, countries, languages);
			addFormEvents(labels);
			alertMessage(labels.formHdrDisclaimer, labels.formTxtDisclaimer, 'info');
		});
	}
}

function initLabels(locale, ui, countries, languages, labelData) {
	document.title = labelData.formPageTitle;

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

	$("#formHdrInternetUsage").append(labelData.formHdrInternetUsage);
	$("#formHdrPersonalInfo").append(labelData.formHdrPersonalInfo);
	$("#formHdrSlogan").prepend(labelData.formHdrSlogan);
	$("#formHdrSubSlogan").append(labelData.formHdrSubSlogan);
	initFormQuestions(labelData);
	$("#formBtnAdd").append(labelData.formBtnAdd);
	$("#formBtnSkip").append(labelData.formBtnSkip);
	initRadioButtons(labelData);
	initCheckboxes(labelData);
	initCountriesSelector(locale, ui, countries);
	initLanguagesSelector(locale, ui, languages);
	initEducationSelector(labelData);
	initInternetSelector(labelData);

	return labels;
}

function initFormQuestions(labelData) {
	$("#formLblBirthDate").append(labelData.formLblBirthDate);
	$("#formLblGender").append(labelData.formLblGender);
	$("#formLblCountry").append(labelData.formLblCountry);
	$("#formLblCommunity").append(labelData.formLblCommunity);
	$("#formLblLanguage").append(labelData.formLblLanguage);
	$("#formLblEducation").append(labelData.formLblEducation);
	$("#formLblMail").append(labelData.formLblMail);
	$("#formLblEmailCheck").append(labelData.formLblEmailCheck);
	$("#formLblSocialNetwork").append(labelData.formLblSocialNetwork);
	$("#formLblInternetUsage").append(labelData.formLblInternetUsage);
	$("#formLblMuseumVisits").append(labelData.formLblMuseumVisits);
	$("#formLblTaggingExperience").append(labelData.formLblTaggingExperience);
	$("#formLblTagSite").append(labelData.formLblTagSite);
}

function initRadioButtons(labelData) {
	$("#formRbtnMale").after(labelData.formRbtnMale);
	$("#formRbtnFemale").after(labelData.formRbtnFemale);
	$("#formRbtnUrban").after(labelData.formRbtnUrban);
	$("#formRbtnSubUrban").after(labelData.formRbtnSubUrban);
	$("#formRbtnRural").after(labelData.formRbtnRural);
	$("#formRbtnNone").after(labelData.formRbtnNone);
	$("#formRbtnNovice").after(labelData.formRbtnNovice);
	$("#formRbtnIntermediate").after(labelData.formRbtnIntermediate);
	$("#formRbtnExpert").after(labelData.formRbtnExpert);
}

function initCheckboxes(labelData) {
	$("#formChkFacebook").after(labelData.formChkFacebook);
	$("#formChkLinkedIn").after(labelData.formChkLinkedIn);
	$("#formChkTwitter").after(labelData.formChkTwitter);
	$("#formChkOther").after(labelData.formChkOther);
	$("#formChkNone").after(labelData.formChkNone);
	$("#formChkTagFlickr").after(labelData.formChkTagFlickr);
	$("#formChkTagDelicious").after(labelData.formChkTagDelicious);
	$("#formChkTagFacebook").after(labelData.formChkTagFacebook);
	$("#formChkTagOther").after(labelData.formChkTagOther);
	$("#formChkTagNone").after(labelData.formChkTagNone);
}

function initCountriesSelector(locale, ui, countries) {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"countries"})
	.then(function(data) {
		for (var key in data) {
			countries[key] = {"name": data[key].name, "country_code": data[key].country_code};
		}

		countries.sort(function(a, b) {
			return a.name.localeCompare(b.name)
		});

		$("#formSltCountry").append($.el.option(""));

		for (var i = 0; i < countries.length; i++) {
			$("#formSltCountry").append($.el.option(countries[i].name));
		}
	}, function(){
		$("#formSltCountry").append($.el.option("No countries found on server"));
	});
}

function initLanguagesSelector(locale, ui, languages) {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"languages"})
	.then(function(data) {
		for (var key in data) {
			languages[key] = {"iso_code":data[key].iso_code, "name":data[key].name};
		}

		languages.sort(function(a,b) {
			return a.name.localeCompare(b.name)
		});

		$("#formSltLanguage").append($.el.option(""));

		for (var i = 0; i < languages.length; i++) {
			$("#formSltLanguage").append($.el.option(languages[i].name));
		}
	}, function(){
		$("#formSltLanguage").append($.el.option("No languages found on server"));
	});
}

function initEducationSelector(labels) {
	$("#formSltEducation").append($.el.option(""));
	$("#formSltEducation").append($.el.option(labels.formOptsEducation.formOptPrimarySchool.label));
	$("#formSltEducation").append($.el.option(labels.formOptsEducation.formOptHighSchool.label));
	$("#formSltEducation").append($.el.option(labels.formOptsEducation.formOptCollege.label));
	$("#formSltEducation").append($.el.option(labels.formOptsEducation.formOptBachelor.label));
	$("#formSltEducation").append($.el.option(labels.formOptsEducation.formOptMaster.label));
	$("#formSltEducation").append($.el.option(labels.formOptsEducation.formOptDoctorate.label));
	$("#formSltEducation").append($.el.option(labels.formOptsEducation.formOptUnkown.label));
}

function initInternetSelector(labels) {
	$("#formSltInternet").append($.el.option(""));
	$("#formSltInternet").append($.el.option(labels.formOptsInternet.formOptInternetAlways.label));
	$("#formSltInternet").append($.el.option(labels.formOptsInternet.formOptInternetOnceADay.label));
	$("#formSltInternet").append($.el.option(labels.formOptsInternet.formOptInternet3to5.label));
	$("#formSltInternet").append($.el.option(labels.formOptsInternet.formOptInternet1to2.label));
	$("#formSltInternet").append($.el.option(labels.formOptsInternet.formOptInternetLessThan1.label));
}

function addButtonEvents(info, labels, countries, languages) {
	$("#formBtnAdd").click(function() {
		processFormFields(info, labels, countries, languages);
	});
	$("#formBtnSkip").click(function() {
		document.location.href = "domain.html";
	});
}

function processFormFields(info, labels, countries, languages) {
	getInput(info, labels, countries, languages);

	var onSuccess = function () {
		document.location.href = "domain.html";
	};

	save_user_info(info, onSuccess);
}

function getInput(info, labels, countries, languages) {
	getInputTextFields(info);
	getInputRadioButtons(info);
	getInputCheckboxes(info);
	getInputDropdownMenus(info, labels, countries, languages);
}

function getInputTextFields(info) {
	if (!($("#formInpAddAge").val() === ""))
		info.age = $("#formInpAddAge").val();
	if (!($("#formInpAddMail").val() === ""))
		info.mail = $("#formInpAddMail").val();
	if (!($("#formInpAddMuseumVisits").val() === ""))
		info.museum_visits = $("#formInpAddMuseumVisits").val();
}

function getInputRadioButtons(info) {
	if(!($("input[name='formRbtnsGender']:checked").val() === undefined))
		info.gender = $("input[name='formRbtnsGender']:checked").val();
	if(!($("input[name='communityRadio']:checked").val() === undefined))
		info.community = $("input[name='communityRadio']:checked").val();
	if(!($("input[name='formRbtnsTaggingExperience']:checked").val() === undefined))
		info.tagging_experience_level = $("input[name='formRbtnsTaggingExperience']:checked").val();
}

function getInputCheckboxes(info) {
	getInputSocialNetwork(info);
	getInputTaggingSite(info);
	getInputEmailCheck(info);
}

function getInputSocialNetwork(info) {
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

function getInputTaggingSite(info) {
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

function getInputEmailCheck(info) {
	if($("#formChkEmail").is(":checked")) {
		info.accurator_email = true;
	} else {
		info.accurator_email = false;
	}
}

function getInputDropdownMenus(info, labels, countries, languages) {
	if (!($("#formSltCountry").val() === ""))
		info.country = getCountryId(countries, $("#formSltCountry").val());
	if (!($("#formSltLanguage").val() === ""))
		info.language = getLanguageCode(languages, $("#formSltLanguage").val());
	if (!($("#formSltEducation").val() === ""))
		info.education = getOptionId(labels.formOptsEducation, $("#formSltEducation").val());
	if (!($("#formSltInternet").val() === ""))
		info.internet_use = getOptionId(labels.formOptsInternet, $("#formSltInternet").val());
}

function getCountryId(countries, name) {
	// Find the geonames id corresponding to the selected name
	for(var i = 0; i < countries.length; i++) {
		if (countries[i].name === name)
			return countries[i].country_code;
	}
}

function getLanguageCode(languages, name) {
	// Find the iso code corresponding to the selected name
	for(var i = 0; i < languages.length; i++) {
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

function addFormEvents(labels) {
	$("#formChkOther").click(function() {
		if(!labels.socialFieldAdded) {
			$("#formDivSocialNetwork").after(
				$.el.div({'class':'form-group'},
						$.el.label({'for':'addSocialSite',
									'id':'frmSocialOpen',
									'class':'col-sm-5 control-label'},
									labels.formLblSocialSiteOpen),
						$.el.div({'class':'col-sm-5'},
								 $.el.input({'type':'text',
									 		 'id':'addSocialSite',
									 		 'class':'form-control'}))));
			labels.socialFieldAdded = true;
		}
	});

	$("#formChkTwitter").click(function() {
		if(!labels.twitterFieldAdded) {
			$("#formDivSocialNetwork").after(
				$.el.div({'class':'form-group'},
						$.el.label({'for':'addTwitterId',
									'id':'formLblTwitterId',
									'class':'col-sm-5 control-label'},
									labels.formLblTwitterId),
						$.el.div({'class':'col-sm-5'},
								 $.el.input({'type':'text',
									 		 'id':'addTwitterId',
									 		 'class':'form-control'}))));
			labels.twitterFieldAdded = true;
		}
	});

	$("#formChkTagOther").click(function() {
		if(!labels.tagsiteFieldAdded) {
			$("#formDivTaggingSite").after(
				$.el.div({'class':'form-group'},
					$.el.label({'for':'addTagSite',
							    'id':'formLblTagSite',
								'class':'col-sm-5 control-label'},
								labels.formLblTagSiteOpen),
					$.el.div({'class':'col-sm-5'},
							 $.el.input({'type':'text',
										 'id':'addTagSite',
										 'class':'form-control'}))));
			labels.tagsiteFieldAdded = true;
		}
	});
}
