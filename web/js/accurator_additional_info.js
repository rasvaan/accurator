/* Accurator Additional Info
*/

function additionalInfoInit() {
	var locale = "en";
	var ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#additional_info";
	
	$.getJSON("ui_text", {locale:locale, ui:ui})
	.done(function(data){
		addButtonEvents();
		initLabels(data);})
	.fail(function(data, textStatus){
//		setRegisterFailureText("Problem connecting to server, please contact the system administrator.");
		});
}

function initLabels(data) {
	$("#txtHeader").prepend(data.txtHeader);
	$("#txtHeaderSub").append(data.txtHeaderSub);
	initFormQuestions(data);
	$("#btnAddInfo").append(data.btnAddInfo);
	$("#btnSkip").append(data.btnSkip);
	initRadioButtons(data);
	initCheckboxes(data);
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
}

function initRadioButtons(data) {
	$("#radioGenderMale").after(data.radioGenderMale);
	$("#radioGenderFemale").after(data.radioGenderFemale);
	$("#radioCommunityUrban").after(data.radioCommunityUrban);
	$("#radioCommunitySubUrban").after(data.radioCommunitySubUrban);
	$("#radioCommunityRural").after(data.radioCommunityRural);
}

function initCheckboxes(data) {
	$("#chkSocialFacebook").after(data.chkSocialFacebook);
	$("#chkSocialLinkedIn").after(data.chkSocialLinkedIn);
	$("#chkSocialTwitter").after(data.chkSocialTwitter);
	$("#chkSocialNone").after(data.chkSocialNone);
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

function addButtonEvents() {
	$("#btnAddInfo").click(function() {
		document.location.href="/profile.html";
	});
	$("#btnSkip").click(function() {
		document.location.href="/profile.html";
	});	
}

