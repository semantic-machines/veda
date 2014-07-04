// Browser-side utility functions

function escape4$(str) {
	if (str) return str.replace(/([ #;?%&,.+*~\':"!^$[\]()=>|\/@])/g,'\\$1');      
	return str;
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
						return "<a href='#/document/" + item["@"] + "'>" + 
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
	$("[data-toggle=popover]")
		.mouseenter( function () {
			var popover_element = $( this );
			uri = popover_element.data("uri");
			var thisTimeout = setTimeout( function () {
				if ($("#"+escape4$(uri)).length) {
					popover_element.popover({
						content: $("#"+escape4$(uri)).text(),
						html: true,
						placement: "auto bottom",
						container: "body"
					}).popover("show");
				} else {
					$.get("/view_popover/" + popover_element.data("uri"), function( data ) {
						$("<div/>", {
							id: uri,
							text: data,
							class: "hide",
						}).appendTo("body");
						popover_element.popover({
							content: data,
							html: true,
							placement: "auto bottom",
							container: "body"
						}).popover("show");
					});
				}
			}, 700);
			popover_element.mouseleave ( function () {
				clearTimeout(thisTimeout);
				popover_element.popover("destroy");
			});
		});
});

