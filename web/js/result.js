/* Result
*  Code for showing the information regarding one of the results
*/
displayOptions = {
	showMetadata: true,
	showAnnotations: true,
	metadataLinkBase: 'results?query='
}

function showResult(uri) {
	metadata(uri);
	annotations(uri);
}

function metadata(uri) {
	if(displayOptions.showMetadata){
		// Get metadata from server
		new Pengine({server: 'pengine',
					application: 'enrichment',
					ask: 'metadata(' + Pengine.stringify(uri, {string:'atom'}) + ', Metadata),!',
					onsuccess: function () {
						appendMetadataWell(this.data, uri);
					}
		});
	}
}

function appendMetadataWell(data, uri) {
	var metadata = data[0].Metadata;
	$("#metadata").append(
		$.el.div({'class':'row'},
			$.el.div({'class':'col-md-10 col-md-offset-1'},
				$.el.div({'class':'well well-sm'},
				  $.el.h4('Showing metadata for ' + metadata.display_title),
					$.el.dl({'class':'dl-horizontal',
							 'id':'metadataList'})))));

	for(var i=0; i<metadata.properties.length; i++) {
		$("#metadataList").append(
			$.el.dt(metadata.properties[i].predicate_label));
		$("#metadataList").append(
			$.el.dd(
				$.el.a({'class':'r_undef',
					    'href':displayOptions.metadataLinkBase +
							   metadata.properties[i].object_label},
					metadata.properties[i].object_label)));
	}
}

function annotations(uri) {
	// Get metadata from server
	if(displayOptions.showAnnotations){
		new Pengine({server: 'pengine',
					application: 'enrichment',
					ask: 'object_annotations(' + Pengine.stringify(uri, {string:'atom'}) + ', Annotations),!',
					onsuccess: function () {
						if(this.data[0].Annotations.annotations.length > 0){
							   $("#metadata").append(annotationWell(this.data));
						}
					}
		});
	}
}

function annotationWell(data) {
	var annotations = data[0].Annotations.annotations;

	$("#metadata").append(
		$.el.div({'class':'row'},
			$.el.div({'class':'col-md-10 col-md-offset-1'},
				$.el.div({'class':'well well-sm'},
				  $.el.h4('Annotations for ' + data[0].Annotations.display_title),
					$.el.dl({'class':'dl-horizontal',
							 'id':'annotationList'})))));


	for(var i=0; i<annotations.length; i++) {
		$("#annotationList").append(
			$.el.dt(annotations[i].field));
		$("#annotationList").append(
			$.el.dd(
				$.el.a({'class':'r_undef',
					    'href':displayOptions.metadataLinkBase +
							   annotations[i].body},
					annotations[i].body)));
	}
}
