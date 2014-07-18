// Browser-side utility functions

function escape4$(str) {
	if (str) return str.replace(/([ #;?%&,.+*~\':"!^$[\]()=>|\/@])/g,'\\$1');      
	return str;
}

// Localize nodeSelector
function localize(container, lang) {
	$.ajax({
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
	});
}

$( function () {
	$("body").on("mouseenter", "[data-toggle='popover']", function () {
		var popover_element = $( this );
		uri = popover_element.attr("href");
		uri = uri.substring(uri.indexOf("#/document/") + "#/document/".length);
		var thisTimeout = setTimeout( function () {
			if ($("#popover_"+escape4$(uri)).length) {
				
				var popover = popover_element.popover({
					content: $("#popover_"+escape4$(uri)).html(),
					html: true,
					placement: "auto",
					container: "body"
				}).popover("show");
				
			} else {
				
				var container = $("<div/>", {
					id: "popover_" + uri,
					class: "hide",
				}).appendTo("body");
				
				new IndividualModel(Veda(), uri, container);
				
				var popover = popover_element.popover({
					content: container.html(),
					html: true,
					placement: "auto",
					container: "body"
				}).popover("show");
				
			}
		}, 700);
		popover_element.mouseleave ( function () {
			clearTimeout(thisTimeout);
			popover_element.popover("destroy");
		});
	});
});

