/*******************************************************************************
Path
*******************************************************************************/
function Path(path, parentId, query) {
    this.path = path; // uris or title element of the path
    this.labels = null; // labels of elements of the path
    this.elements = []; // elements of the path
    this.node = null;
    this.parentId = parentId; // id of the parent element (probably cluster)
    this.query = null; // the query which is the start of the path

    this.init(query);
}

Path.prototype.init = function(query) {
    this.node = this.html();
    if (query) ? this.query = query : this.query = "query";
    if (typeof this.path === 'string' || this.path instanceof String) {
        this.addTitle();
    }
}

Path.prototype.html = function() {
    return $.el.div({'class':'row path'},
                $.el.div({'class':'col-md-12'},
                    $.el.h4()));
}

Path.prototype.addSubject = function() {
    $(this.node).find("h4").append(
        $.el.span({'class':'path-label path-property'},
                  this.elements[this.elements.length-2].label),
        $.el.span({'class':'path-label path-resource'},
                  this.elements[this.elements.length-3].label)
    );
}

Path.prototype.addTitle = function() {
    $(this.node).find("h4").append(
        $.el.span({'class':'path-label path-resource'},
                  this.path)
    );
}

Path.prototype.enrich = function() {
    var _path = this;

    // no need for enriching if we have a title
    if (typeof this.path === 'string' || this.path instanceof String) return;

	return $.ajax({type: "POST",
		url: "metadata",
		contentType: "application/json",
		data: JSON.stringify({"uris":this.path, "type":"label"})})
	.then(function(labels) {
        _path.labels = labels;

		for (var i=0; i<_path.path.length; i++) {
			_path.elements[i] = {
                uri:_path.path[i],
                label:truncate(_path.labels[i], 50)
            };
		}

        _path.elements.reverse();
		_path.addSubject();
		_path.unfoldEvent();
	});
}

Path.prototype.unfoldEvent = function() {
    var _path = this;
    var pathHtml = $.el.h4();

    for (var i=0; i<this.elements.length; i++) {
        // label colouring
        if (i==0) {
			pathHtml.appendChild(
				$.el.span({'class':'path-label path-literal'},
                    this.query));
		} else if (i%2==0) {
            pathHtml.appendChild(
                $.el.span({'class':'path-label path-resource'},
                          this.elements[i].label));
        } else {
            pathHtml.appendChild(
                $.el.span({'class':'path-label path-property'},
                          this.elements[i].label));
        }
        // add arrow if not end of path
        if(!(this.elements.length == i+1)){
            pathHtml.appendChild(
                $.el.span({'class':'glyphicon glyphicon-arrow-right'}));
        }
    }
    // add event
    $(this.node).find(".path-label").click(function() {
        $(_path.node).html(
            $.el.div({'class':'col-md-12'},
                pathHtml)
        );
    });
}
