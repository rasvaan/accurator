/*******************************************************************************
Accurator Profile
Code for showing statistical elements on the profile page and allowing the user
to change settings.
*******************************************************************************/
"use strict";

function profileInit() {
	var locale = getLocale();
	var domain = getDomain();

	populateFlags(locale);

	userLoggedIn()
	.then(function(userData) {
		// user is logged in, so draw page
		drawPage(userData);
	}, function() {
		// user is not logged in, show modal
		var onDismissal = function() {
			document.location.href = "intro.html";
		};

		login(drawPage, onDismissal);
	});

	function drawPage(userData) {
		var domainData;
		var user = userData.user;
		var userName = getUserName(user);
		var realName = userData.real_name;

		setLinkLogo("profile");
		populateNavbar(userName, [], locale);

		domainSettings(domain)
		.then(function(data) {
			domainData = data;
			return getLabels(locale, domainData.hasUI + "profile");
		})
		.then(function(labelData) {
			var labels = initLabels(labelData, realName);

			addDomainTitle(domainData, locale, labels);
			addButtonEvents(user);
			populateRecentlyAnnotated(user, labels);
		});
	}
}

function populateRecentlyAnnotated(user, labels) {
	$.getJSON("annotations", {
		uri:user,
		type:"user"
	})
	.then(function(uris){
		if (uris.length === 0) {
			$("#profileDivLastAnnotated").hide();
		} else {
			//TODO: limit length of uris (faster if someone annotated a bunch)?
			var noRecentlyAnnotatedItems = 6;

			var cluster = new Cluster(
				"profileDivCluster",
				uris,
				labels.profileLblLastAnnotated
			);

			cluster.init(noRecentlyAnnotatedItems)
			.then(function() {
				$("#profileDivLastAnnotated").append(cluster.node);
			});
		}
	});
}

function initLabels(labelData, realName) {
	// add retrieved labels to html elements
	document.title = labelData.profilePageTitle;

	var labels = {
		profileLblLastAnnotated: labelData.profileLblLastAnnotated,
		profileTxtDomain: labelData.profileTxtDomain
	};

	// check if real name is available
	if (typeof realName !== 'undefined') {
		$("#profileHdrSlogan").prepend(labelData.profileHdrSlogan + " " + realName + " ");
	} else {
		$("#profileHdrSlogan").prepend(labelData.profileHdrSlogan);
	}
	$("#profileTxtSubSlogan").prepend(labelData.profileTxtSubSlogan);
	$("#profileTxtStartAnnotating").append(labelData.profileTxtStartAnnotating);
	$("#navbarBtnRecommend").append(labelData.navbarBtnRecommend);
	$("#navbarBtnSearch").append(labelData.navbarBtnSearch);
	$("#profileBtnDomain").prepend(labelData.profileBtnDomain);

	return labels;
}

function addDomainTitle(domainData, locale, labels) {
	// add the title of the current domain to the profile page
	getLabels(locale, domainData.hasLabel)
	.then(function(data){
		$("#profileTxtDomain").append(
			labels.profileTxtDomain,
			$.el.span({'class':'text-info'}, data.textLabel)
		);
	});
}

function addButtonEvents(user) {
	$("#navbarBtnRecommend").click(function() {
		document.location.href = "results.html?user=" + user;
	});

	$("#profileBtnDomain").click(function() {
		document.location.href = "domain.html";
	});

	// Search on pressing enter
	$("#navbarInpSearch").keypress(function(event) {
		if (event.which == 13) {
			var query = encodeURIComponent($("#navbarInpSearch").val());
			document.location.href = "results.html?query=" + query;
		}
	});
	$("#navbarBtnSearch").click(function() {
		var query = encodeURIComponent($("#navbarInpSearch").val());
		document.location.href = "results.html?query=" + query;
	});
	$("#profileBtnChangeExpertise").click(function() {
		document.location.href = "expertise.html";
	});
}
