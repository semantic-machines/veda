// Document Presenter

Veda(function DocumentPresenter(veda) { "use strict";

	// Get templates
	var template = $("#document-template").html();
	var single_property = $("#single-property-template").html();
	
	veda.on("document:loaded", function (document, container_param) {
		
		var container = container_param || $("#main");
		container.html(template);
		
		// Render document title
		try {
			var label = document["rdfs:label"].values.reduce(function(p, c) {
					if (c.lang != veda.user.language) return p;
					return p != "" ? p + ", " + c.data : c.data;
				}, "");
			label = label || document["@"]; 
			label += " <small><a href='#/document/" + document["@"] + "'><i class='glyphicon glyphicon-share-alt'></i></a></small>";
			$("#label", container).html( label );
		} catch (e) {
			$("#label", container).html( document["@"] + " <small><a href='#/document/" + document["@"] + "'><i class='glyphicon glyphicon-share-alt'></i></a></small>" );
		}
		
		// Render document properties
		document.properties.map ( function (property_uri) {
			if (property_uri == "@") return;
			
			try {
				var property = veda.cache[property_uri] ? JSON.parse(veda.cache[property_uri]) : get_individual(veda.ticket, property_uri);
				
				var property_label = property["rdfs:label"].reduce(function(p, c) {
						if (c.lang != veda.user.language) return p;
						return p != "" ? p + ", " + c.data : c.data;
					}, "");
				property_label = property_label || property["@"];
				property_label = "<a href='#/document/" + property["@"] + "'>" + property_label + "</a>";
			} catch (e) {
				property_label = property_uri;
			}
			
			var property_values = Object.create(document[property_uri].values);
			for (var i in property_values) {
				if (property_values[i].type != "Uri") {
					if (property_values[i].type == "String" && property_values[i].lang != veda.user.language && property_values[i].lang != "NONE") {
						property_values[i] = "";
						continue;
					}
					property_values[i] = property_values[i].data; 
					continue;
				}
				if (property_values[i].data.indexOf("://") >= 0) {
					property_values[i] = "<a href='"+ property_values[i].data +"'>" + property_values[i].data + "</a>";
					continue;
				}
				try {
					var value = veda.cache[property_values[i].data] ? JSON.parse(veda.cache[property_values[i].data]) : get_individual(veda.ticket, property_values[i].data);
					var value_label = value["rdfs:label"].reduce(function(p, c) {
							if (c.lang != veda.user.language && c.lang != "NONE") return p;
							return p != "" ? p + ", " + c.data : c.data;
						}, "");
					value_label = value_label || value["@"];
					value_label = "<a href='#/document/" + value["@"] + "'>" + value_label + "</a>";
					property_values[i] = value_label;
				} catch (e) {
					property_values[i] = property_values[i].data;
				}
			}
			$("#document-properties", container).append(
				riot.render(
					single_property,
					{
						property: property_label,
						values: property_values.reduce(function(p,c) { return p != "" ? p + ", " + c : c; }, "")
					}
				)
			);
		});
		
		
		/*
		var expanded = {};
		for (var property_uri in document.individual) {
			expanded[property_uri] = {};
			if (property_uri == "@") {
				expanded["@"] = document.individual["@"];
				continue;
			}
			var property = veda.cache[property_uri] ? JSON.parse( veda.cache[property_uri] ) : get_individual(veda.ticket, property_uri);
			var values = document.individual[property_uri];
			for (var i in values) {
				if (values[i].type != "Uri") continue;
				if (values[i].data.indexOf("://") >= 0) continue; // Link to external resource
				values[i] = veda.cache[values[i].data] ? JSON.parse( veda.cache[values[i].data] ) : get_individual(veda.ticket, values[i].data);
			}
			expanded[property_uri]["property"] = property;
			expanded[property_uri]["values"] = values;
		}
		
		// Localize document with preferred language
		var doc = JSON.parse(
			JSON.stringify( expanded )

			// Filter localized substrings like {"lang":"EN", "data":"data123", "type":"type123"}
			.replace(/{[^\[{]*?"lang":"([A-Z]{2,4})"[^}\]]*?}/g, function (match, lang) {
				// Strict localization
				// return lang == veda.user.language ? match : "";
				
				// Non-strict localization
				return lang == veda.user.language || lang == "NONE" ? match : "";
			})
			
			// Filter possible residues like "[," ",]" "[,]"
			.replace(/\[(,)|(,)\]|\[(,)\]/g, function (match, p1, p2, p3) {
				return p1 ? "[" : p2 ? "]" : "[]";
			})
		);

		// Render document title
		
		try {
			$("#label", container).html( 
				
				doc["rdf:type"]["values"].reduce(function(p, c) {
					return p + c["rdfs:label"].reduce(function(p, c) { return p != "" ? p + ", " + c.data : c.data}, "");
				}, "") 
				
				+ ": " +
				
				doc["rdfs:label"]["values"].reduce(function(p, c) {
					return p != "" ? p + ", " + c.data : c.data;
				}, "")
				
				+ " <small><a href='#/document/" + doc["@"] + "'><i class='glyphicon glyphicon-share-alt'></i></a></small>"
				
			);
		} catch (e) {
			$("#label", container).html( 
				doc["@"] + " <small><a href='#/document/" + doc["@"] + "'><i class='glyphicon glyphicon-share-alt'></i></a></small>"
			);
		}

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

		*/

	});

});
