/* Accurator Additional Info
*/
var locale, domain, experiment, ui;
var countries = [];
var languages = [];
var educationOptions,  internetOptions;
var info = {};
var twitterFieldAdded = false;
var tagsiteFieldAdded = false;
var socialFieldAdded = false;
var frmTwitterId, frmTagSiteOpen, frmSocialSiteOpen;
var txtDisclaimer, txtDisclaimerTitle;

function additionalInfoInit() {
	locale = getLocale();
	domain = getDomain();
	experiment = getExperiment();

	populateFlags(locale);

	// Make sure user is logged in
	onLoggedIn = function(loginData){
		setLinkLogo("profile");

		//Get domain settings before populating ui
		onDomain = function(domainData) {
			ui = domainData.ui + "additional_info";
			populateUI();
			var userName = getUserName(loginData.user);
			populateNavbar(userName, [{link:"profile.html", name:"Profile"}]);
		};
		domainSettings = domainSettings(domain, onDomain);
	};
	onDismissal = function(){document.location.href="intro.html"};
	logUserIn(onLoggedIn, onDismissal);
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
	.done(function(data){
		addButtonEvents();
		initLabels(data);
		addFormEvents();
		alertMessage(txtDisclaimerTitle, txtDisclaimer, 'info');
	});
}

function addButtonEvents() {
	$("#btnAddInfo").click(function() {
		processFormFields();
	});
	$("#btnSkip").click(function() {
		document.location.href="domain.html";
	});
}

function initLabels(labels) {
	document.title = labels.title;
	$("#pnlInternetUsage").append(labels.pnlInternetUsage);
	$("#pnlPersonalInfo").append(labels.pnlPersonalInfo);
	frmTwitterId = labels.frmTwitterId;
	frmTagSiteOpen = labels.frmTagSiteOpen;
	frmSocialSiteOpen = labels.frmSocialSiteOpen;
	$("#txtHeader").prepend(labels.txtHeader);
	$("#txtHeaderSub").append(labels.txtHeaderSub);
	initFormQuestions(labels);
	$("#btnAddInfo").append(labels.btnAddInfo);
	$("#btnSkip").append(labels.btnSkip);
	initRadioButtons(labels);
	initCheckboxes(labels);
	initCountriesSelector();
	initLanguagesSelector();
	educationOptions = labels.educationOptions;
	initEducationSelector();
	internetOptions = labels.internetOptions;
	initInternetSelector(labels.internetOptions);
	txtDisclaimer = labels.txtDisclaimer;
	txtDisclaimerTitle = labels.txtDisclaimerTitle;
}

function initFormQuestions(data) {
	$("#frmBirthDate").append(data.frmBirthDate);
	$("#frmGender").append(data.frmGender);
	$("#frmCountry").append(data.frmCountry);
	$("#frmCommunity").append(data.frmCommunity);
	$("#frmLanguage").append(data.frmLanguage);
	$("#frmEducation").append(data.frmEducation);
	$("#frmMail").append(data.frmMail);
	$("#frmMailCheck").append(data.frmMailCheck);
	$("#frmSocialNetwork").append(data.frmSocialNetwork);
	$("#frmInternetUsage").append(data.frmInternetUsage);
	$("#frmMuseumVisits").append(data.frmMuseumVisits);
	$("#frmTaggingExperienceLevel").append(data.frmTaggingExperienceLevel);
	$("#frmTagSite").append(data.frmTagSite);
}

function initRadioButtons(data) {
	$("#radioGenderMale").after(data.radioGenderMale);
	$("#radioGenderFemale").after(data.radioGenderFemale);
	$("#radioCommunityUrban").after(data.radioCommunityUrban);
	$("#radioCommunitySubUrban").after(data.radioCommunitySubUrban);
	$("#radioCommunityRural").after(data.radioCommunityRural);
	$("#radioTaggingNone").after(data.radioTaggingNone);
	$("#radioTaggingNovice").after(data.radioTaggingNovice);
	$("#radioTaggingIntermediate").after(data.radioTaggingIntermediate);
	$("#radioTaggingExpert").after(data.radioTaggingExpert);
}

function initCheckboxes(data) {
	$("#chkSocialFacebook").after(data.chkSocialFacebook);
	$("#chkSocialLinkedIn").after(data.chkSocialLinkedIn);
	$("#chkSocialTwitter").after(data.chkSocialTwitter);
	$("#chkSocialOther").after(data.chkSocialOther);
	$("#chkSocialNone").after(data.chkSocialNone);
	$("#chkTagSiteFlickr").after(data.chkTagSiteFlickr);
	$("#chkTagSiteDelicious").after(data.chkTagSiteDelicious);
	$("#chkTagSiteFacebook").after(data.chkTagSiteFacebook);
	$("#chkTagSiteOther").after(data.chkTagSiteOther);
	$("#chkTagNone").after(data.chkTagNone);
}

function addFormEvents() {
	$("#chkSocialOther").click(function() {
		if(!socialFieldAdded) {
			$("#frmGroupSocialNetwork").after(
				$.el.div({'class':'form-group'},
						$.el.label({'for':'addSocialSite',
									'id':'frmSocialOpen',
									'class':'col-sm-5 control-label'},
									frmSocialSiteOpen),
						$.el.div({'class':'col-sm-5'},
								 $.el.input({'type':'text',
									 		 'id':'addSocialSite',
									 		 'class':'form-control'}))));
			socialFieldAdded = true;
		}
	});
	$("#chkSocialTwitter").click(function() {
		if(!twitterFieldAdded) {
			$("#frmGroupSocialNetwork").after(
				$.el.div({'class':'form-group'},
						$.el.label({'for':'addTwitterId',
									'id':'frmTwitterId',
									'class':'col-sm-5 control-label'},
									frmTwitterId),
						$.el.div({'class':'col-sm-5'},
								 $.el.input({'type':'text',
									 		 'id':'addTwitterId',
									 		 'class':'form-control'}))));
			twitterFieldAdded = true;
		}
	});
	$("#chkTagSiteOther").click(function() {
		if(!tagsiteFieldAdded) {
			$("#frmGroupTaggingSite").after(
				$.el.div({'class':'form-group'},
					$.el.label({'for':'addTagSite',
							    'id':'frmTagSite',
								'class':'col-sm-5 control-label'},
								frmTagSiteOpen),
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

		$("#sltCountry").append($.el.option(""));
		for (var i=0; i<countries.length; i++) {
			$("#sltCountry").append($.el.option(countries[i].name));
		}
	})
	.fail(function(data, textStatus){
		$("#sltLanguage").append($.el.option("No countries found on server"));
	});
}

function initLanguagesSelector() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"languages"})
	.done(function(data){
		for (var key in data) {
			languages[key] = {"iso_code":data[key].iso_code, "name":data[key].name};
		}
		languages.sort(function(a,b) { return a.name.localeCompare(b.name) });

		$("#sltLanguage").append($.el.option(""));
		for (var i=0; i<languages.length; i++) {
			$("#sltLanguage").append($.el.option(languages[i].name));
		}
	})
	.fail(function(data, textStatus){
		$("#sltLanguage").append($.el.option("No languages found on server"));
	});
}

function initEducationSelector() {
	$("#sltEducation").append($.el.option(""));
	$("#sltEducation").append($.el.option(educationOptions.optionPrimarySchool.label));
	$("#sltEducation").append($.el.option(educationOptions.optionHighSchool.label));
	$("#sltEducation").append($.el.option(educationOptions.optionCollege.label));
	$("#sltEducation").append($.el.option(educationOptions.optionBachelor.label));
	$("#sltEducation").append($.el.option(educationOptions.optionMaster.label));
	$("#sltEducation").append($.el.option(educationOptions.optionDoctorate.label));
	$("#sltEducation").append($.el.option(educationOptions.optionUknown.label));
}

function initInternetSelector(optionList) {
	$("#sltInternet").append($.el.option(""));
	$("#sltInternet").append($.el.option(internetOptions.internetAlways.label));
	$("#sltInternet").append($.el.option(internetOptions.internetOnceADay.label));
	$("#sltInternet").append($.el.option(internetOptions.internet3to5.label));
	$("#sltInternet").append($.el.option(internetOptions.internet1to2.label));
	$("#sltInternet").append($.el.option(internetOptions.internetLessThan1.label));
}

function processFormFields() {
	getInput();
	var onSuccess = function(){				       document.location.href="domain.html";
	}
	save_user_info(info, onSuccess);
}

function getInput() {
	getInputTextFields();
	getInputRadioButtons();
	getInputCheckboxes();
	getInputDropdownMenus();
}

function getInputTextFields() {
	if (!($("#addAge").val() === ""))
		info.age = $("#addAge").val();
	if (!($("#addMail").val() === ""))
		info.mail = $("#addMail").val();
	if (!($("#addMuseumVisits").val() === ""))
		info.museum_visits = $("#addMuseumVisits").val();
}

function getInputRadioButtons() {
	if(!($("input[name='genderRadio']:checked").val() === undefined))
		info.gender = $("input[name='genderRadio']:checked").val();
	if(!($("input[name='communityRadio']:checked").val() === undefined))
		info.community = $("input[name='communityRadio']:checked").val();
	if(!($("input[name='taggingExperienceLevelRadio']:checked").val() === undefined))
		info.tagging_experience_level = $("input[name='taggingExperienceLevelRadio']:checked").val();
}

function getInputCheckboxes() {
	getInputSocialNetwork();
	getInputTaggingSite();
	getInputEmailCheck();
}

function getInputSocialNetwork() {
	if($("#chkSocialNone").is(":checked")) {
		info.facebook = false;
		info.linked_in = false;
		info.twitter = false;
	} else {
		if ($("#chkSocialFacebook").is(":checked"))
			info.facebook = true;
		if ($("#chkSocialLinkedIn").is(":checked"))
			info.linked_in = true;
		//Set twitter to true when no id is given but box is checked
		if ($("#chkSocialTwitter").is(":checked")) {
			if (!($("#addTwitterId").val() === undefined) && !($("#addTwitterId").val() === "")) {
				info.twitter = $("#addTwitterId").val();
			} else {
				info.twitter = true;
			}
		}
		//Set social to true
		if ($("#chkSocialOther").is(":checked")) {
			if (!($("#addSocialSite").val() === undefined) && !($("#addSocialSite").val() === "")) {
				info.other_social_site = $("#addSocialSite").val();
			} else {
				info.other_social_site = true;
			}
		}
	}
}

function getInputTaggingSite() {
	if($("#chkTagNone").is(":checked")) {
		info.flickr = false;
		info.delicious = false;
		info.tag_facebook = false;
		info.other = false;
	} else {
		if ($("#chkTagSiteFlickr").is(":checked"))
			info.flickr = true;
		if ($("#chkTagSiteDelicious").is(":checked"))
			info.delicious = true;
		if ($("#chkTagSiteFacebook").is(":checked"))
			info.tag_facebook = true;

		//Set tagsite to true
		if ($("#chkTagSiteOther").is(":checked")) {
			if (!($("#addTagSite").val() === undefined) && !($("#addTagSite").val() === "")) {
				info.other_tag_site = $("#addTagSite").val();
			} else {
				info.other_tag_site = true;
			}
		}
	}
}

function getInputEmailCheck() {
	if($("#chkEmail").is(":checked")) {
		info.accurator_email = true;
	} else {
		info.accurator_email = false;
	}
}

function getInputDropdownMenus() {
	if (!($("#sltCountry").val() === ""))
		info.country = getCountryId($("#sltCountry").val());
	if (!($("#sltLanguage").val() === ""))
		info.language = getLanguageCode($("#sltLanguage").val());
	if (!($("#sltEducation").val() === ""))
		info.education = getOptionId(educationOptions, $("#sltEducation").val());
	if (!($("#sltInternet").val() === ""))
		info.internet_use = getOptionId(internetOptions, $("#sltInternet").val());
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
