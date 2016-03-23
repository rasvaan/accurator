/*******************************************************************************
Path
*******************************************************************************/
function Path(uris, parentId) {
    this.uris = uris; // uris of elements of the path
    this.labels = null; // labels of elements of the path
    this.elements = []; // elements of the path
    this.node = null;
    this.parentId = parentId; // id of the parent element (probably cluster)

    this.init();
}

Path.prototype.init = function() {
    this.node = this.html();
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

Path.prototype.enrich = function() {
    var _path = this;

	return $.ajax({type: "POST",
		url: "metadata",
		contentType: "application/json",
		data: JSON.stringify({"uris":this.uris, "type":"label"})})
	.then(function(labels) {
        _path.labels = labels;

		for (var i=0; i<_path.uris.length; i++) {
			_path.elements[i] = {
                uri:_path.uris[i],
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
        //TODO: Get the query?
                        //   query));
                        "query"));
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
