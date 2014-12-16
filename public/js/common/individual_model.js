// Document Model

;(function (veda) { "use strict";

	/* 
	 * Параметр noCache используется для создания нового индивида, 
	 * если параметр отсутствует (чаще всего) либо равен false,
	 * делается попытка извлечения объект из кэша.
	 */
	
	veda.IndividualModel = function (uri, noCache) {
		
		var self = riot.observable(this);

		// Define Model functions
		var individual = {};
		var original_individual = "{}";
		var properties = {};
		var values = {};
		self.properties = {};

		self.defineProperty = function (property_uri, getter, setter) {
			
			//properties[property_uri] = undefined;
			if (properties[property_uri]) return;
			
			Object.defineProperty(self.properties, property_uri, {
				get: function () { 
					if (properties[property_uri]) return properties[property_uri];
					try { properties[property_uri] = new veda.IndividualModel(property_uri); } 
					catch (e) { properties[property_uri] = property_uri; }
					return properties[property_uri];
				},
				
				set: function (value) { 
					if (properties[property_uri] == value) return; 
					properties[property_uri] = value; 
				},
				
				configurable: true
			
			});
			
			var filteredStrings = [];
			values[property_uri] = undefined;
			
			Object.defineProperty(self, property_uri, {
				get: function () { 
					if (values[property_uri]) return values[property_uri];
					if (!individual[property_uri]) individual[property_uri] = [];
					values[property_uri] = individual[property_uri].map( function (value) {
						if (value.type == "String") {
							var string = new String(value.data);
							string.language = value.lang == "NONE" ? undefined : value.lang;
							return (string.language == undefined || string.language in veda.user.language) ? (
								string
							) : (
								filteredStrings.push(string),
								undefined
							);
						} 
						else if (value.type == "Uri") {
							if (value.data.search(/^.{3,5}:\/\//) == 0) return new String(value.data);
							return new veda.IndividualModel(value.data);
						} 
						else if (value.type == "Datetime") return new Date(Date.parse(value.data));
						else if (value.type == "Decimal") return new Number(value.data);
						else if (value.type == "Integer") return new Number(value.data);
						else if (value.type == "Boolean") return new Boolean(value.data);
						else throw ("Unsupported type of property value");
					});
					// Filter undefined values
					values[property_uri] = values[property_uri].filter(function (item) { return item });
					
					if (getter) getter(values[property_uri]);
					
					return values[property_uri];
				},
				
				set: function (value) { 
					values[property_uri] = value;
					individual[property_uri] = values[property_uri].concat(filteredStrings).map( function (value) {
						var result = {};
						if (value instanceof Number || typeof value === "number" ) {
							result.type = isInteger(value.valueOf()) ? "Integer" : "Decimal";
							result.data = value.valueOf();
							return result;
						} else if (value instanceof Boolean || typeof value === "boolean") {
							result.type = "Boolean";
							result.data = value.valueOf();
							return result;
						} else if (value instanceof String || typeof value === "string") {
							result.type = "String";
							result.data = value.valueOf();
							result.lang = value.language || "NONE";
							return result;
						} else if (value instanceof Date) {
							result.type = "Datetime";
							result.data = value.toISOString();
							return result;
						} else if (value instanceof veda.IndividualModel) {
							result.type = "Uri";
							result.data = value.id;
							return result;
						} else {
							return value;
						}
					});
					if (setter) setter(values[property_uri]);
					/*self.trigger("individual:propertyChanged:" + property_uri, values[property_uri]);*/
				},
				
				configurable: true
			
			});
			
		}

		function isInteger (n) { return n % 1 === 0; }
		
		self.load = function (uri) {
			self.trigger("individual:beforeLoad");
			if (typeof uri == "string") {
				if (veda.cache[uri] && !noCache) {
					self = veda.cache[uri];
					self.trigger("individual:afterLoad");
					return;
				}
				try {
					individual = get_individual(veda.ticket, uri);
				} catch (e) {
					individual = {
						"@": uri,
						"rdfs:label": [{type: "String", data: uri, lang: "NONE"}]
					}
				}
			} else if (uri instanceof veda.IndividualModel) {
				self = uri;
				self.trigger("individual:afterLoad");
				return;
			} else {
				individual = uri;
			}
			original_individual = JSON.stringify(individual);
			Object.keys(individual).map(function (property_uri) {
				if (property_uri == "@") return;
				if (property_uri == "rdf:type") return;
				self.defineProperty(property_uri);
			});
			if (!noCache) veda.cache[self.id] = self;
			self.trigger("individual:afterLoad");
		};

		self.save = function() {
			self.trigger("individual:beforeSave");
			Object.keys(individual).reduce(function (acc, property_uri) {
				self[property_uri];
				if (property_uri == "@") return acc;
				acc[property_uri] = individual[property_uri].filter(function (item) {
					return item && item.data !== "";
				});
				if (!acc[property_uri].length) delete acc[property_uri];
				return acc;
			}, individual);
			try { 
				put_individual(veda.ticket, individual);
			} catch (e) {
				alert("Error: " + e.status + "\n" + "description: " + e.description);
			}
			original_individual = JSON.stringify(individual);
			self.trigger("individual:afterSave", original_individual);
		};

		self.reset = function () {
			self.trigger("individual:beforeReset");
			individual = JSON.parse(original_individual);
			properties = {};
			self.properties = {};
			values = {};
			Object.keys(individual).map(function (property_uri) {
				if (property_uri == "@") return;
				self.defineProperty(property_uri);
			});
			self.trigger("individual:afterReset");
		};

		if (!uri) { individual["@"] = guid();
			original_individual = '{"@":"' + individual["@"] +'"}';
		}
		
		Object.defineProperty(self, "id", {
			get: function () { 
				return individual["@"];
			},
			set: function (value) { 
				individual["@"] = value;
			}
		});

		self.defineProperty("rdf:type", undefined, function (classes) {
			classes.map(function (_class) {
				Object.keys(_class.domainProperties).map(function (property_uri) {
					self.defineProperty(property_uri);
				});
			});
			self.trigger("individual:typeChanged", classes);
		});
		
		// Load data 
		if (uri) self.load(uri); 
		
		return self;
	};

}(veda));
