/**
 * @class veda.IndividualModel 
 * 
 * This class is used to manipulate with individuals.
 */
veda.Module(function IndividualModel(veda) { "use strict";

	//var cnt = 0;
	/**
	 * @constructor
	 * @param {String} uri URI of individual. If not specified, than id of individual will be generated automatically. 
	 * @param {boolean} noCache turn cache off. If false or not set, than object will be return from browser cache. If true or individual not found in cache - than individual will be requested from database 
	 */
	veda.IndividualModel = function (uri, noCache) {
		//console.log("individual model:", ++cnt, uri);
		
		var self = riot.observable(this);

		// Define Model functions
		var individual = {};
		var original_individual = "{}";
		var properties = {};
		var values = {};
		self.properties = {};

		/**
		 * @method
		 * 
		 * You must define property in individual  
		 * 
		 * @param {String} property_uri name of property
		 * @param {Function} getterCB callback function that called after get method executed
		 * @param {Function} setterCB callback function that called after set method executed
		 */
		self.defineProperty = function (property_uri, getterCB, setterCB) {
			
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
					
					if (getterCB) getterCB(values[property_uri]);
					
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
					if (setterCB) setterCB(values[property_uri]);
					self.trigger("individual:propertyModified", property_uri, values[property_uri]);
				},
				
				configurable: true
			
			});
			
		}

		function isInteger (n) { return n % 1 === 0; }
		
		/**
		 * @method
		 * Load individual specified by uri from database. If noCache parameter (from constructor) is not true, than try to load individual from browser cache first.
		 * @param {String} uri individual uri
		 */
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
				// TODO may be delete (this part of code is not clear)
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
				if (property_uri == "v-s:deleted") return;
				self.defineProperty(property_uri);
			});
			if (!noCache) veda.cache[self.id] = self;
			self.trigger("individual:afterLoad");
		};

		/**
		 * @method
		 * Save current individual to database
		 */
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

		/**
		 * @method
		 * Reset current individual to database
		 */
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

		/**
		 * @method
		 * @param {String} property_uri property name
		 * @return {boolean} is requested property exists in this individual
		 */
		self.hasValue = function (property_uri) {
			return !!(self[property_uri] && self[property_uri].length);
		};

		if (!uri) { individual["@"] = guid();
			original_individual = '{"@":"' + individual["@"] +'"}';
			if (veda.cache && !noCache) {
				veda.cache[individual["@"]] = self;
			}
		}
		
		/**
		 * @method
		 * Initialize individual with class specific domain properties and methods
		 */
		self.init = function () {
			self["rdf:type"].map(function (_class) {
				if (_class.domainProperties) {
					Object.keys(_class.domainProperties).map(function (property_uri) {
						self.defineProperty(property_uri);
					});
				}
				if (_class.model) {
					var model = new Function (
						"individual", 
						_class.model["v-s:script"][0] + "//# sourceURL=" + _class.id + "Model.js"
					);
					model(self);
				}
			});
		}

		// Special properties
		Object.defineProperty(self, "id", {
			get: function () { 
				return individual["@"];
			},
			set: function (value) { 
				individual["@"] = value;
			}
		});

		self.defineProperty("rdf:type", undefined, function (classes) {
			self.init();
			self.trigger("individual:typeChanged", classes);
		});

		var rights;
		Object.defineProperty(self, "rights", {
			get: function () { 
				if (rights) return rights;
				try {
					var rightsJSON = get_rights(veda.ticket, self.id);
					rights = new veda.IndividualModel( rightsJSON );
				} catch (e) {
					rights = null;
				} finally {
					return rights;
				}
			},
			configurable: false
		});

		self.defineProperty("v-s:deleted");
		
		// Load data 
		if (uri) self.load(uri); 
		
		return self;
	};

});
