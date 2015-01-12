/* Accurator Additional Info
*/

var txtHeader, txtHeaderSub,
	btnAddInfo, btnSkip,
	frmAge, frmGender, frmCountry,
	radioGenderFemale, radioGenderMale;

function additionalInfoInit() {
	var locale = "en";
	var ui = "http://semanticweb.cs.vu.nl/accurator/ui/bird#additional_info";
	
	$.getJSON("ui_text", {locale:locale, ui:ui})
	.done(function(data){
		addButtonEvents();
		initVariables(data);
		addTextHeader();
		addTextForm();
		addTextButtons();})
	.fail(function(data, textStatus){
//		setRegisterFailureText("Problem connecting to server, please contact the system administrator.");
		});
	
	function initVariables(data) {
		txtHeader = data.txtHeader;
		txtHeaderSub = data.txtHeaderSub;
		btnAddInfo = data.btnAddInfo;
		btnSkip = data.btnSkip;
		frmAge = data.frmAge;
		frmGender = data.frmGender;
		radioGenderMale = data.radioGenderMale;
		radioGenderFemale = data.radioGenderFemale;
		frmCountry = data.frmCountry;
	}
	
	function addTextHeader() {
		$("#txtHeader").prepend(txtHeader);
		$("#txtHeaderSub").append(txtHeaderSub);
	}
	
	function addTextForm() {
		$("#frmAge").append(frmAge);
		$("#frmGender").append(frmGender);
		$("#radioGenderMale").after(radioGenderMale);
		$("#radioGenderFemale").after(radioGenderFemale);
		$("#frmCountry").append(frmCountry);
		
	}
	
	function addTextButtons() {
		$("#btnAddInfo").append(btnAddInfo);
		$("#btnSkip").append(btnSkip);
	}
	
	function addButtonEvents() {
		$("#btnAddInfo").click(function() {
			document.location.href="/profile.html";
		});
		$("#btnSkip").click(function() {
			document.location.href="/profile.html";
		});	
	}
}
