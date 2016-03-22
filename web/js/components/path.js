/*******************************************************************************
Path
*******************************************************************************/
function Path(uris, labels, elements) {
    this.uris = uris;
    this.labels = labels;
    this.elements = elements;
    this.node = null;

    this.init();
}

Path.prototype.init = function() {
    this.node = this.html();
}

Path.prototype.html = function() {
    return $.el.div({'class':'row path'},
                $.el.div({'class':'col-md-12'},
                    $.el.h4(
                        $.el.span({'class':'path-label path-property'},
                                  this.elements[this.elements.length-2].label),
                        $.el.span({'class':'path-label path-resource'},
                                  this.elements[this.elements.length-3].label)
    )));
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
