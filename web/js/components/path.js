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
		// path.unfoldEvent("#cluster" + clusterId, query);
	});
}

Path.prototype.unfoldEvent = function(id, query) {
    var pathHtml = $.el.h4();

    for(var i=0; i<elements.length; i++) {
        // Label colouring
        if(i==0){
			pathHtml.appendChild(
				$.el.span({'class':'path-label path-literal'},
				          query));
		} else if(i%2==0){
            pathHtml.appendChild(
                $.el.span({'class':'path-label path-resource'},
                          elements[i].label));
        } else {
            pathHtml.appendChild(
                $.el.span({'class':'path-label path-property'},
                          elements[i].label));
        }
        // Add arrow if not end of path
        if(!(elements.length == i+1)){
            pathHtml.appendChild(
                $.el.span({'class':'glyphicon glyphicon-arrow-right'}));
        }
    }
    // Add event
    $(id + " .path-label").click(function() {
        $(id + " .path").html(
            $.el.div({'class':'col-md-12'},
                pathHtml)
        );
    });
}
