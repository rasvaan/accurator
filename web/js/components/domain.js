/******************************************************************************
Domain

Code for representing domains
*******************************************************************************/
function Domain(id, title, image, imageBrightness) {
	console.log(arguments);
	this.id = id; // id of domain
	this.title = title; // title of domain
	this.image = image; // image illustrating domain
	this.imageBrightness = imageBrightness; // is the descriptive image dark or light
	this.link = null; // URL of location where someone can show expertise of domain
	// this.subDomains = subDomains;
	this.node = null;  // html of thumbnail

	this.init();
}

Domain.prototype.init = function() {
	this.node = $.el.div({'class':'noPadding col-md-6'},
		$.el.h3({'class':'domainHdr',
				 'id':'domainTxt' + this.id},
				 this.title),
		$.el.img({'class':'domainImg',
				  'id':'domainImg' + this.id,
				  'src':this.image})
	);

	this.setTitleColor();
	this.setClickEvent();
}

Domain.prototype.setTitleColor = function() {
	if(this.imageBrightness === "dark")
		$(this.node).find("#domainTxt" + this.id).css('color', '#fff');
}

Domain.prototype.setClickEvent = function() {
	$(this.node).find("#domainImg" + this.id).click(function() {
		setDomain(this.id)
		.then(function() {
			document.location.href = "results.html";
		});
	});
	$(this.node).find("#domainTxt" + this.id).click(function() {
		setDomain(this.id)
		.then(function() {
			document.location.href = "results.html";
		});
	});
}
