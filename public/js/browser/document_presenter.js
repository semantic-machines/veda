// Document Presenter

Veda(function DocumentPresenter(veda) { "use strict";

	veda.on("document:loaded", function (document) {

		// Localize document with preferred language
		var doc = JSON.parse(
			JSON.stringify( document.get_expanded() )
			.replace(/{[^{]*?"lang":"(.{2})"[^}]*?}/g, function (match, lang) {
				return lang == veda.user.language ? match : "";
			})
			.replace(/\[(,)|(,)\]|\[(,)\]/g, function (match, p1, p2, p3) {
				return p1 ? "[" : p2 ? "]" : "[]";
			})
		);

		// Get templates
		var template = $("#document-template").html();
		$("#main").html(template);
		var single_property = $("#single-property-template").html();
		
		// Render document to screen
		for (var i in doc) {
			if (i == "@") continue;
			var property = doc[i]["property"]["rdfs:label"].reduce(function(previous, current) { return previous!="" ? previous + ", " + current.data : current.data; }, "");
			var values = doc[i]["values"].reduce(function(previous, current) {
				if (!current["@"]) return previous != "" ? previous + ", " + current.data : current.data;
				return previous + current["rdfs:label"].reduce(function(p,c){ return p != "" ? p + ", " + c.data : c.data}, "");
			}, "");
			$("#document #doc").append( 
				riot.render(
					single_property, 
					{ 
						property: property,
						values: values
					}
				) 
			);
		}

	});

});
