// Browser-side utility functions

function escape4$(str) {
	if (str) return str.replace(/([ #;?%&,.+*~\':"!^$[\]()=>|\/@])/g,'\\$1');      
	return str;
}

// Localize nodeSelector
function localize(container, lang) {
	/*$.ajax({
		type: "GET",
		dataType: "script",
		url: "js/i18n/vocabulary_"+lang.toLowerCase()+".js",
		async: false,
		cache: true,
	}).done(function( msg ) {
		eval(msg);
		$('[i18n]', container).each(function() {
			$(this).text($.i18n._($(this).attr('i18n')));
		});
	});*/
}

