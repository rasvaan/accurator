/* Accurator Additional Info
*/
var locale;
var ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#additional_info";
var twitterFieldAdded = false;
var frmTwitterId;

function additionalInfoInit() {
	locale = getLocale();
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
	.done(function(data){
		addButtonEvents();
		initLabels(data);
		addFormEvents();
		$("#frmAge").focus();})
	.fail(function(data, textStatus){
		});
}

function addButtonEvents() {
	$("#btnAddInfo").click(function() {
		document.location.href="/expertise.html";
	});
	$("#btnSkip").click(function() {
		document.location.href="/expertise.html";
	});	
}

function initLabels(data) {
	frmTwitterId = data.frmTwitterId;
	$("#txtHeader").prepend(data.txtHeader);
	$("#txtHeaderSub").append(data.txtHeaderSub);
	initFormQuestions(data);
	$("#btnAddInfo").append(data.btnAddInfo);
	$("#btnSkip").append(data.btnSkip);
	initRadioButtons(data);
	initCheckboxes(data);
	initCountriesSelector();
	initLanguagesSelector();
	initEducationSelector(data.educationOptions);
	initIncomeSelector(data.incomeOptions);
	initInternetSelector(data.internetOptions);
}

function initFormQuestions(data) {
	$("#frmAge").append(data.frmAge);
	$("#frmGender").append(data.frmGender);
	$("#frmCountry").append(data.frmCountry);
	$("#frmCommunity").append(data.frmCommunity);
	$("#frmLanguage").append(data.frmLanguage);
	$("#frmEducation").append(data.frmEducation);
	$("#frmIncome").append(data.frmIncome);
	$("#frmMail").append(data.frmMail);
	$("#frmSocialNetwork").append(data.frmSocialNetwork);
	$("#frmInternetUsage").append(data.frmInternetUsage);
	$("#frmMuseumVisits").append(data.frmMuseumVisits);
	$("#frmEmployee").append(data.frmEmployee);	
	$("#frmTaggingExperience").append(data.frmTaggingExperience);
	$("#frmTaggingExperienceLevel").append(data.frmTaggingExperienceLevel);
	$("#frmTagSite").append(data.frmTagSite);
}

function initRadioButtons(data) {
	$("#radioGenderMale").after(data.radioGenderMale);
	$("#radioGenderFemale").after(data.radioGenderFemale);
	$("#radioCommunityUrban").after(data.radioCommunityUrban);
	$("#radioCommunitySubUrban").after(data.radioCommunitySubUrban);
	$("#radioCommunityRural").after(data.radioCommunityRural);
	$("#radioEmployeeYes").after(data.radioEmployeeYes);
	$("#radioEmployeeNo").after(data.radioEmployeeNo);
	$("#radioTaggingYes").after(data.radioTaggingYes);
	$("#radioTaggingNo").after(data.radioTaggingNo);
	$("#radioTaggingNovice").after(data.radioTaggingNovice);
	$("#radioTaggingIntermediate").after(data.radioTaggingIntermediate);
	$("#radioTaggingExpert").after(data.radioTaggingExpert);
}

function initCheckboxes(data) {
	$("#chkSocialFacebook").after(data.chkSocialFacebook);
	$("#chkSocialLinkedIn").after(data.chkSocialLinkedIn);
	$("#chkSocialTwitter").after(data.chkSocialTwitter);
	$("#chkSocialNone").after(data.chkSocialNone);
	$("#chkTagSiteWaIsDa").after(data.chkTagSiteWaIsDa);
	$("#chkTagSiteSpotvogel").after(data.chkTagSiteSpotvogel);
	$("#chkTagSiteSteve").after(data.chkTagSiteSteve);
	$("#chkTagNone").after(data.chkTagNone);
}

function addFormEvents() {
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
}

function initCountriesSelector() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"countries"})
	.done(function(data){
		var countries = [];
		for (var key in data) {
			countries[key] = {"name":data[key].name, "geo_id":data[key].geo_id};
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
		var languages = [];
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

function initEducationSelector(optionList) {
	$("#sltEducation").append($.el.option(""));
	$("#sltEducation").append($.el.option(optionList.optionPrimarySchool));
	$("#sltEducation").append($.el.option(optionList.optionHighSchool));
	$("#sltEducation").append($.el.option(optionList.optionCollege));
	$("#sltEducation").append($.el.option(optionList.optionBachelor));
	$("#sltEducation").append($.el.option(optionList.optionMaster));
	$("#sltEducation").append($.el.option(optionList.optionDoctorate));
	$("#sltEducation").append($.el.option(optionList.optionUknown));
}

function initIncomeSelector(optionList) {
	$("#sltIncome").append($.el.option(""));
	$("#sltIncome").append($.el.option(optionList.income20));
	$("#sltIncome").append($.el.option(optionList.income20to35));
	$("#sltIncome").append($.el.option(optionList.income35to50));
	$("#sltIncome").append($.el.option(optionList.income50to75));
	$("#sltIncome").append($.el.option(optionList.income75to100));
	$("#sltIncome").append($.el.option(optionList.income100to150));
	$("#sltIncome").append($.el.option(optionList.income150to200));
	$("#sltIncome").append($.el.option(optionList.income200));
}

function initInternetSelector(optionList) {
	$("#sltInternet").append($.el.option(""));
	$("#sltInternet").append($.el.option(optionList.internetAlways));
	$("#sltInternet").append($.el.option(optionList.internetOnceADay));
	$("#sltInternet").append($.el.option(optionList.internet3to5));
	$("#sltInternet").append($.el.option(optionList.internet1to2));
	$("#sltInternet").append($.el.option(optionList.internetLessThan1));
}