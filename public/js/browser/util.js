// Browser-side utility functions

function escape4$(str) {
	if (str) return str.replace(/([ #;?%&,.+*~\':"!^$[\]()=>|\/@])/g,'\\$1');      
	return str;
}

// Localize nodeSelector
function localize(nodeSelector, lang) {
	$.ajax({
		type: "GET",
		dataType: "script",
		url: "js/i18n/vocabulary_"+lang.toLowerCase()+".js"
	}).done(function( msg ) {
		eval(msg);
		$(nodeSelector + ' .i18n').each(function() {
			$(this).text($.i18n._($(this).attr('label')));
		});
	});
}

// Helper to render a single property
function renderProperty (veda, individual, property_uri, template, container) {
	var label, uri, values;
	label = typeof individual.properties[property_uri] == "object" ? 
				individual.properties[property_uri]["rdfs:label"]
					.filter(function(item){return item.language == veda.user.language || item.language == "NONE"})
					.join(", ")
				: individual.properties[property_uri];
	uri = typeof individual.properties[property_uri] == "object" ? individual.properties[property_uri]["@"] : "";
	values = individual[property_uri]
				.map( function (item) {
					if (item instanceof String)
						// Check if string starts with http:// or ftp://
						return item.search(/^.{3,5}:\/\//) == 0 ? "<a target='_blank' href='" + item + "'>" + item + "</a>" : item ;
					else if (item instanceof IndividualModel)
						return "<a data-toggle='popover' href='#/document/" + item["@"] + "'>" + 
							(item["rdfs:label"] ? item["rdfs:label"].filter(function(item){return item.language == veda.user.language || item.language == "NONE"}).join(", ") : item["@"]) + "</a>";
					else return item;
				})
				.filter(function(item){return item.language ? item.language == veda.user.language || item.language == "NONE" : item})
				.join(", ");
	container.append(
		riot.render(
			template,
			{
				label: label,
				uri: uri,
				values: values
			}
		)
	);
}

$( function () {
	$("body").on("mouseenter", "[data-toggle='popover']", function () {
		var popover_element = $( this );
		uri = popover_element.attr("href");
		uri = uri.substring(uri.indexOf("#/document/") + "#/document/".length);
		var thisTimeout = setTimeout( function () {
			if ($("#"+escape4$(uri)).length) {
				
				var popover = popover_element.popover({
					content: $("#popover_"+escape4$(uri)).text(),
					html: true,
					placement: "auto",
					container: "body"
				}).popover("show");
				
			} else {
				
				var container = $("<div/>", {
					id: "popover_" + uri,
					class: "hide",
				}).appendTo("body");
				
				new DocumentModel(app, uri, container);
				
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

