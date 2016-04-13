/******************************************************************************
Domain

Code for representing domains
*******************************************************************************/
function Domain(id, title, image, imageBrightness, subDomains) {
	this.id = id; // id of domain
	this.title = title; // title of domain
	this.image = image; // image illustrating domain
	this.imageBrightness = imageBrightness; // is the descriptive image dark or light
	this.link = null; // URL of location where someone can show expertise of domain
	this.subDomains = subDomains;
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

	this.setLink();
	this.setTitleColor();
	this.setClickEvent();
}

Domain.prototype.setLink = function() {
	if (this.subDomains.length > 0) {
		this.link = "topics.html";
	} else {
		this.link = "results.html";
	}
}

Domain.prototype.setTitleColor = function() {
	if(this.imageBrightness === "dark")
		$(this.node).find("#domainTxt" + this.id).css('color', '#fff');
}

Domain.prototype.setClickEvent = function() {
	var _domain = this;
	console.log("Setting event to link ", this.link);
	$(this.node).find("#domainImg" + this.id).click(function() {
		setDomain(_domain.id)
		.then(function() {
			document.location.href = _domain.link;
		});
	});
	$(this.node).find("#domainTxt" + this.id).click(function() {
		setDomain(_domain.id)
		.then(function() {
			document.location.href = "results.html";
		});
	});
}
