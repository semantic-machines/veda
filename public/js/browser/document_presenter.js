// Document Presenter

Veda(function DocumentPresenter(veda) { "use strict";

	veda.on("document:loaded", function (document) {

		// Localize document with preferred language
		
		var doc = JSON.parse(

			JSON.stringify( document.get_expanded() )

			// Filter localized substrings like {"lang":"EN", "data":"data123", "type":"type123"}
			.replace(/{[^{]*?"lang":"(.{2})"[^}]*?}/g, function (match, lang) {
				return lang == veda.user.language ? match : "";
			})
			
			// Filter residues like "[," ",]" "[,]"
			.replace(/\[(,)|(,)\]|\[(,)\]/g, function (match, p1, p2, p3) {
				return p1 ? "[" : p2 ? "]" : "[]";
			})
		);

		// Get templates
		var template = $("#document-template").html();
		$("#main").html(template);
		var single_property = $("#single-property-template").html();
		
		// Render document to screen
		$("#document #label").html( 
			
			// Document title (type: label)
			doc["rdf:type"]["values"].reduce(function(p, c) {
				return p + c["rdfs:label"].reduce(function(p, c) { return p != "" ? p + ", " + c.data : c.data}, "");
			}, "") 
			
			+ ": "+
			
			doc["rdfs:label"]["values"].reduce(function(p, c) {
				return p != "" ? p + ", " + c.data : c.data;
			}, "")
			
		);
		
		// Document properties (property: values)
		for (var i in doc) {
			if (i == "@") continue;
			
			var property = doc[i]["property"]["rdfs:label"].reduce(function(p, c) { 
				return p!="" ? p + ", " + c.data : c.data; 
			}, "");
			
			var values = doc[i]["values"].reduce(function(p, c) {
				if (!c["@"]) return p != "" ? p + ", " + c.data : c.data;
				return p + c["rdfs:label"].reduce(function(p1, c1) { return p1 != "" ? p1 + ", " + "<a href='/#/document/" + c["@"] + "'>" + c1.data + "</a>" : "<a href='/#/document/" + c["@"] + "'>" + c1.data + "</a>" }, "");
			}, "")
			
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
