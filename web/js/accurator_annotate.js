/* Accurator Annotate
*/

function annotateInit() {
	alert('Accurator Annotate');
	$.ajax({url: "annotate", body_only: "true", target:"http://purl.org/collections/nl/naturalis/print-135002"})
	.done(function(data){
		  console.log(data);
	});
}
