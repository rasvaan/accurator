/* Accurator Results
*/
var locale, userName;

function resultsInit() {
	locale = getLocale();
	onSuccess = function(data){
		setLinkLogo("profile");
		user = data.user;
		userName = getUserName(user);
		populateNavbar(userName, [{link:"profile.html", name:"Profile"}]);
		var query = getParameterByName("query");
		var userParam = getParameterByName("user");
		
		//Provide results based on query or recommend something. In case of no in put recommend based on retrieved user.
		if(query != "") {
			initiateSearch(query);
		} else if(userParam != "") {
			recommendItems(userParam);
		} else {
			recommendItems(user);
		}
	};
	onFail = function(){document.location.href="intro.html";};
	logUserIn(onSuccess, onFail);
}

function initiateSearch(query) {
	search(query);
}

function recommendItems(user) {
	alert("Should be recommending items to  " + user);
}