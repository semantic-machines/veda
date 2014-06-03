// Document Presenter

Veda(function DocumentPresenter(veda) { "use strict";

	veda.on("document:loaded", function (document, container_param) {
		
		// Localize document with preferred language
		var doc = JSON.parse(
			JSON.stringify( document.get_expanded() )

			// Filter localized substrings like {"lang":"EN", "data":"data123", "type":"type123"}
			.replace(/{[^\[{]*?"lang":"([A-Z]{2,4})"[^}\]]*?}/g, function (match, lang) {
				// Strict localization
				//return lang == veda.user.language ? match : "";
				
				// Non-strict localization
				return lang == veda.user.language || lang == "NONE" ? match : "";
			})
			
			// Filter possible residues like "[," ",]" "[,]"
			.replace(/\[(,)|(,)\]|\[(,)\]/g, function (match, p1, p2, p3) {
				return p1 ? "[" : p2 ? "]" : "[]";
			})
		);

		// Get templates
		var template = $("#document-template").html();
		var container = container_param || $("#main");
		container.html(template);
		
		var single_property = $("#single-property-template").html();
		
		// Render document title
		$("#label", container).html( 
			
			doc["rdf:type"]["values"].reduce(function(p, c) {
				return p + c["rdfs:label"].reduce(function(p, c) { return p != "" ? p + ", " + c.data : c.data}, "");
			}, "") 
			
			+ ": " +
			
			doc["rdfs:label"]["values"].reduce(function(p, c) {
				return p != "" ? p + ", " + c.data : c.data;
			}, "")
			
			+ " <small><a href='#/document/" + document.individual["@"] + "'><i class='glyphicon glyphicon-share-alt'></i></a></small>"
			
		);
		
		// Render document properties
		for (var i in doc) {
			if (i == "@") continue;
			
			if (doc[i]["property"]["@"]) { 
				var property = doc[i]["property"]["rdfs:label"].reduce(function(p, c) { 
					return p != "" ? p + ", " + "<a href='#/document/" + doc[i]["property"]["@"] + "'>" + c.data + "</a>" : "<a href='#/document/" + doc[i]["property"]["@"] + "'>" + c.data + "</a>"; 
				}, "");
			} else { 
				var property = i;
			}
			
			var values = doc[i]["values"].reduce(function(p, c) {
				if (c.type == "Uri" && c.data.indexOf("://") >= 0 ) return p != "" ? p + ", " + "<a href='" + c.data + "'>" + c.data + "</a>" : "<a href='" + c.data + "'>" + c.data + "</a>" ;
				if (!c["@"]) return p != "" ? p + ", " + c.data : c.data;
				return p != "" ? 
					p + ", " + c["rdfs:label"].reduce(function(p1, c1) { return p1 != "" ? p1 + ", " + "<a href='#/document/" + c["@"] + "'>" + c1.data + "</a>" : "<a href='#/document/" + c["@"] + "'>" + c1.data + "</a>" }, "")
					:
					c["rdfs:label"].reduce(function(p1, c1) { return p1 != "" ? p1 + ", " + "<a href='#/document/" + c["@"] + "'>" + c1.data + "</a>" : "<a href='#/document/" + c["@"] + "'>" + c1.data + "</a>" }, "");
			}, "")
	
			$("#document-properties", container).append(
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
