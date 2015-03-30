/* Accurator Annotate
*/
var query, locale, domain, user, ui, uri;
var vntFirstTitle, vntFirstText;

displayOptions = {
	showAnnotations: true,
	metadataLinkBase: 'results.html?query='
}

function annotateInit() {
	locale = getLocale();
	domain = getDomain();
	uri = getParameterByName("uri");
	
	// Make sure user is logged in
	onLoggedIn = function(loginData) {
		setLinkLogo("profile");
		
		//Get domain settings before populating ui
		onDomain = function(domainData) {
			ui = domainData.ui + "annotate";
			populateUI();
			user = loginData.user;
			var userName = getUserName(loginData.user);
			populateNavbar(userName, [{link:"profile.html", name:"Profile"}]);
		};
		domainSettings = domainSettings(domain, onDomain);
	};
	// If user is not logged go to intro page
	onDismissal = function(){document.location.href="intro.html";};
	logUserIn(onLoggedIn, onDismissal);
}

function populateUI() {
	$.getJSON("ui_elements", {locale:locale, ui:ui, type:"labels"})
	.done(function(labels){
		document.title = labels.title;
		initLabels(labels);
		addPath();
		addButtonEvents();
		events();
	});
	console.log(uri);
	showResult(uri);
}

function initLabels(data) {
	$("#btnPrevious").append(data.btnPrevious);
	$("#btnNext").prepend(data.btnNext);
	$("#btnAnnotateRecommend").append(data.btnAnnotateRecommend);
	$("#btnAnnotateSearch").append(data.btnAnnotateSearch);
	vntFirstTitle = data.vntFirstTitle;
	vntFirstText = data.vntFirstText;
}

function events() {
	$.getJSON("recently_annotated", {user:user})
	.done(function(annotations){
		uris = annotations.uris;
		if(uris.length===0) {
			alertMessage(vntFirstTitle, vntFirstText, 'success');
		}
	});
}

function addPath() {
	query = localStorage.getItem("query");
	var cluster = JSON.parse(localStorage.getItem("currentCluster"));
	$("#path").append(pathHtmlElements(cluster.path));
	unfoldPathEvent("#path", cluster.path);
}

function addButtonEvents() {
	var index = parseInt(localStorage.getItem("itemIndex"));
	var cluster = JSON.parse(localStorage.getItem("currentCluster"));
	var items = cluster.items;
	
	console.log(index, items.length);
	if(index === 0) {
		$("#btnPrevious").attr("disabled", "disabled");
	} else {
		$("#btnPrevious").click(function() {
			localStorage.setItem("itemIndex", index - 1);
			document.location.href= "annotate_image.html?uri=" + items[index -1].uri;
		});
	}
	
	if(index === items.length-1) {
		$("#btnNext").attr("disabled", "disabled");
	} else {
		$("#btnNext").click(function() {
			localStorage.setItem("itemIndex", index + 1);
			document.location.href= "annotate_image.html?uri=" + items[index + 1].uri;
		});
	}
	
	$("#btnAnnotateRecommend").click(function() {
		document.location.href="results.html" + "?user=" + user;
	});
	// Search on pressing enter
	$("#frmSearch").keypress(function(event) {
		if (event.which == 13) {
			var query = encodeURIComponent($("#frmSearch").val());
			document.location.href="results.html?query=" + query;
		}
	});
	$("#btnAnnotateSearch").click(function() {
		var query = encodeURIComponent($("#frmSearch").val());
		document.location.href="results.html?query=" + query;
	});
}