/**
 * @class veda.IndividualModel
 *
 * This class is used to manipulate with individuals.
 */
veda.Module(function (veda) { "use strict";

	/**
	 * @constructor
	 * @param {String} uri URI of individual. If not specified, than id of individual will be generated automatically.
	 * @param {String/jQuery} container Container to render individual in. If passed as String, then must be a valid css selector. If passed as jQuery, then is used as is. If not specified, than individual will not be presented.
	 * @param {String/jQuery/veda.IndividualModel} template Template to render individual with.
	 * @param {String} mode Initial mode for individual presenter. Expected values: "view", "edit", "search".
	 * @param {boolean} cache Use cache true / false. If true or not set, then object will be return from application cache (veda.cache). If false or individual not found in application cache - than individual will be loaded from database
	 * @param {boolean} init individual with class model at load. If true or not set, then individual will be initialized with class specific model upon load.
	 */
	veda.IndividualModel = function (uri, container, template, mode, cache, init) {

		var self = riot.observable(this);

		// Define Model functions
		this._ = {};
		this._.cache = typeof cache !== "undefined" ? cache : true;
		this._.init = typeof init !== "undefined" ? init : true;
		this._.isNew = false;
		this._.isSync = false;
		this.properties = {};
		this._.filtered = {};

		if (!uri) {
			this._.isNew = true;
			var id = veda.Util.genUri();
			this.properties["@"] = id;
			if (this._.cache && veda.cache) {
				veda.cache[id] = this;
			}
		}

		function typeHandler (property_uri, values) {
			if (property_uri === "rdf:type") {
				this._.isSync = false;
				this.init();
				this.trigger("individual:typeChanged", values);
			}
		}
		this.on("individual:propertyModified", typeHandler);

		this.on("individual:beforeSave", function () {
			var now = new Date();
			var editor = veda.appointment ? veda.appointment : veda.user;
			this["v-s:edited"] = [ now ];
			this["v-s:lastEditor"] = [ editor ];
			if (!this.hasValue("v-s:created")) this["v-s:created"] = [ now ];
			if (!this.hasValue("v-s:publisher")) this["v-s:publisher"] = [ editor ];
		});

		if (container) {
			this.on("individual:afterLoad", function (individual) {
				this.present.call(individual, container, template, mode);
			});
			this.on("individual:typeChanged", function () {
				this.present(container, template, mode);
			});
		}

		// Load data
		if (uri) self = self.load(uri);
		else self.trigger("individual:afterLoad", self);

		return self;
	};

	var proto = veda.IndividualModel.prototype;

	// Define properties from ontology in veda.IndividualModel.prototype
	veda.IndividualModel.defineProperty = function (property_uri) {
		Object.defineProperty(proto, property_uri, {
			get: function () {
				var self = this;
				if (!self.properties[property_uri]) return [];
				var values = self.properties[property_uri]
					.filter(function (value) {
						var condition = value.type !== "String" || value.lang === "NONE" || (veda.user && veda.user.language && value.lang in veda.user.language);
						if (condition === false) {
							var filtered = self._.filtered[property_uri] || [],
								  found = filtered.filter(function (filteredVal) {
									  return filteredVal.data === value.data && filteredVal.lang === value.lang;
								  });
							if ( !found.length ) {
								filtered.push( value );
							}
							self._.filtered[property_uri] = filtered;
						}
						return condition;
					})
					.map( parser );
				return values;
			},
			set: function (values) {
				this._.isSync = false;
				var notNull = values.filter(function (i) { return i !== null });
				var serialized = notNull.map( serializer );
				if (this._.filtered[property_uri] && this._.filtered[property_uri].length) {
					serialized = serialized.concat( this._.filtered[property_uri] );
				}
				this.properties[property_uri] = serialized;
				this.trigger("individual:propertyModified", property_uri, notNull);
			},
			configurable: false,
			enumerable: false
		});
	}

	function parser(value) {
		if (value.type === "String") {
			var string = new String(value.data);
			if (value.lang !== "NONE") { string.language = value.lang };
			return string;
		} else if (value.type === "Uri") {
			if (value.data.search(/^.{3,5}:\/\//) === 0) return value.data;
			return new veda.IndividualModel(value.data);
		} else if (value.type === "Datetime") {
			return new Date(Date.parse(value.data));
		} else {
			return value.data;
		}
	}

	function serializer (value) {
		if (typeof value === "number" ) {
			return {
				type: isInteger(value) ? "Integer" : "Decimal",
				data: value
			}
		} else if (typeof value === "boolean") {
			return {
				type: "Boolean",
				data: value
			}
		} else if (typeof value === "string" || value instanceof String) {
			return {
				type: "String",
				data: value.valueOf(),
				lang: value.language || "NONE"
			}
		} else if (value instanceof Date) {
			return {
				type: "Datetime",
				data: value.toISOString()
			}
		} else if (value instanceof veda.IndividualModel) {
			return {
				type: "Uri",
				data: value.id
			}
		} else {
			return value;
		}
	}

	function isInteger (n) { return n % 1 === 0; }

	// Special properties
	Object.defineProperty(proto, "id", {
		get: function () {
			return this.properties["@"];
		},
		set: function (value) {
			this._.isNew = false;
			this._.isSync = false;
			this.properties["@"] = value;
			this.trigger("individual:idChanged", value);
		}
	});

	Object.defineProperty(proto, "rights", {
		get: function () {
			if (this._.rights) return this._.rights;
			if (this._.isNew) {
				this._.rights = new veda.IndividualModel(undefined, undefined, undefined, undefined, false);
				this._.rights["v-s:canRead"] = [ true ];
				this._.rights["v-s:canUpdate"] = [ true ];
				this._.rights["v-s:canDelete"] = [ true ];
				return this._.rights;
			}
			try {
				var rightsJSON = get_rights(veda.ticket, this.id);
				this._.rights = new veda.IndividualModel( rightsJSON, undefined, undefined, undefined, false );
			} catch (e) {
				this._.rights = null;
			} finally {
				return this._.rights;
			}
		},
		configurable: false,
		enumerable: false
	});

	Object.defineProperty(proto, "rightsOrigin", {
		get: function () {
			if (this._.rightsOrigin) return this._.rightsOrigin;
			try {
				var rightsOriginArr = get_rights_origin(veda.ticket, this.id);
				this._.rightsOrigin = rightsOriginArr.map(function (item) {
					return new veda.IndividualModel( item, undefined, undefined, undefined, false );
				});
			} catch (e) {
				this._.rightsOrigin = null;
			} finally {
				return this._.rightsOrigin;
			}
		},
		configurable: false,
		enumerable: false
	});

	/**
	 * @method
	 * Load individual specified by uri from database. If cache parameter (from constructor) is true, than try to load individual from browser cache first.
	 * @param {String} uri individual uri
	 */
	proto.load = function (uri) {
		var self = this;
		self.trigger("individual:beforeLoad");
		if (typeof uri === "string") {
			if (self._.cache && veda.cache[uri]) {
				self.trigger("individual:afterLoad", veda.cache[uri]);
				return veda.cache[uri];
			}
			try {
				self.properties = get_individual(veda.ticket, uri);
				self._.isNew = false;
				self._.isSync = true;
			} catch (e) {
				self.properties = {
					"@": uri,
					"rdfs:label": [{type: "String", data: uri, lang: "NONE"}],
					"rdf:type": [{type: "Uri", data: "rdfs:Resource"}]
				};
			}
		} else if (uri instanceof veda.IndividualModel) {
			self.trigger("individual:afterLoad", uri);
			return uri;
		} else {
			self.properties = uri;
		}
		if (self._.cache) veda.cache[self.id] = self;
		if (self._.init) self.init();
		self.trigger("individual:afterLoad", self);
		return this;
	};

	/**
	 * @method
	 * Save current individual to database (with validation and adding new version)
	 */
	proto.save = function(parent) {
		var self = this;
		self.trigger("individual:beforeSave");
		// Do not save individual to server if nothing changed
		//if (self._.isSync) return;
		if ( this.hasValue("v-s:isDraft") && this["v-s:isDraft"][0] == true ) {
			veda.drafts.remove(this.id);
		}
		Object.keys(self.properties).reduce(function (acc, property_uri) {
			if (property_uri === "@") return acc;
			acc[property_uri] = self.properties[property_uri].filter(function (item) {
				return item && item.data !== "";
			});
			if (!acc[property_uri].length) delete acc[property_uri];
			return acc;
		}, self.properties);
		try {
			put_individual(veda.ticket, this.properties);
		} catch (e) {
			this.draft(parent);
		}
		this._.isNew = false;
		this._.isSync = true;
		if (this._.cache) veda.cache[this.id] = self;
		this.trigger("individual:afterSave");
		return this;
	}

	/**
	 * @method
	 * Save current individual without validation and without adding new version
	 */
	proto.draft = function(parent) {
		this.trigger("individual:beforeDraft");
		veda.drafts.set(this.id, this, parent);
		this.trigger("individual:afterDraft");
		return this;
	}

	/**
	 * @method
	 * Reset current individual to database
	 */
	proto.reset = function () {
		var self = this;
		self.trigger("individual:beforeReset");
		if ( this.hasValue("v-s:isDraft") && this["v-s:isDraft"][0] == true ) {
			veda.drafts.remove(this.id);
		}
		this._.filtered = {};
		var original;
		try {
			original = get_individual(veda.ticket, this.id);
		} catch (e) {
			original = {};
		}
		Object.getOwnPropertyNames(self.properties).map(function (property_uri) {
			if (property_uri === "@" || property_uri === "rdf:type") { return; }
			if (original[property_uri] && original[property_uri].length) {
				self[property_uri] = original[property_uri].map( parser );
			} else {
				self[property_uri] = [];
			}
		});
		self._.isNew = false;
		self._.isSync = true;
		self.trigger("individual:afterReset");
		return this;
	};

	/**
	 * @method
	 * Update current individual with values from database & merge with local changes
	 */
	proto.update = function () {
		var self = this;
		self.trigger("individual:beforeUpdate");
		var original;
		try {
			original = get_individual(veda.ticket, this.id);
		} catch (e) {
			original = {};
		}
		Object.getOwnPropertyNames(self.properties).map(function (property_uri) {
			if (property_uri === "@" || property_uri === "rdf:type") { return; }
			if (original[property_uri] && original[property_uri].length) {
				self[property_uri] = original[property_uri].map( parser );
			}
		});
		self._.isNew = false;
		self._.isSync = true;
		self.trigger("individual:afterUpdate");
		return this;
	};

	/**
	 * @method
	 * Mark current individual as deleted in database (add v-s:deleted property)
	 */
	proto.delete = function (parent) {
		this.trigger("individual:beforeDelete");
		if ( this.hasValue("v-s:isDraft") && this["v-s:isDraft"][0] == true ) {
			veda.drafts.remove(this.id);
		}
		this["v-s:deleted"] = [ true ];
		this.save(parent);
		this.trigger("individual:afterDelete");
		return this;
	};

	/**
	 * @method
	 * Recover current individual in database (remove v-s:deleted property)
	 */
	proto.recover = function (parent) {
		this.trigger("individual:beforeRecover");
		if ( this.hasValue("v-s:isDraft") && this["v-s:isDraft"][0] == true ) {
			veda.drafts.remove(this.id);
		}
		this["v-s:deleted"] = [];
		this.save(parent);
		this.trigger("individual:afterRecover");
		return this;
	};

	/**
	 * @method
	 * @param {String} property_uri property name
	 * @return {boolean} is requested property exists in this individual
	 */
	proto.hasValue = function (property_uri) {
		return !!(this[property_uri] && this[property_uri].length);
	};

	/**
	 * @method
	 * @param {String} id of class to check
	 * @return {boolean} is individual rdf:type subclass of requested class
	 */
	proto.is = function (_class) {
		if (typeof _class.valueOf() === "string") {
			_class = new veda.IndividualModel(_class);
		}
		return this["rdf:type"].reduce(function (acc, item) {
			return acc || (item.id === _class.id) || isSubClassOf(item, _class);
		}, false);
	};
	function isSubClassOf(_class, _super) {
		if (!_class.hasValue("rdfs:subClassOf")) return false;
		return _class["rdfs:subClassOf"].reduce(function (acc, item) {
			return acc || (item.id === _super.id) || isSubClassOf(item, _super);
		}, false);
	}

	/**
	 * @method
	 * Initialize individual with class specific domain properties and methods
	 */
	proto.init = function () {
		var self = this;
		self["rdf:type"].map(function (_class) {
			if (_class.model) {
				var model = new Function (
					_class.model["v-s:script"][0] + "//# sourceURL=" + _class.id + "Model.js"
				);
				model.call(self);
			}
		});
		return this;
	};

	/**
	 * @method
	 * Clone individual with different (generated) id
	 * @return {veda.IndividualModel} clone of this individual with different id.
	 */
	proto.clone = function (uri) {
		var individual = JSON.parse( JSON.stringify(this.properties) );
		individual["@"] = veda.Util.genUri();
		var clone = new veda.IndividualModel(individual);
		return clone;
	};

	/**
	 * @method
	 * Check whether individual is synchronized with db
	 * @return {boolean}
	 */
	proto.isSync = function () {
		return this._.isSync;
	};

	/**
	 * @method
	 * Check whether individual is new (not saved in db)
	 * @return {boolean}
	 */
	proto.isNew = function () {
		return this._.isNew;
	};

	/**
	 * @method
	 * Call individual presenter
	 * @param {String/jQuery} container Container to render individual in. If passed as String, then must be a valid css selector. If passed as jQuery, then is used as is. If not specified, than individual will not be presented.
	 * @param {String/jQuery/veda.IndividualModel} template Template to render individual with.
	 * @param {String} mode Initial mode for individual presenter. Expected values: "view", "edit", "search".
	 */
	proto.present = function (container, template, mode) {
		if (container) {
			if (!this.hasValue("rdf:type")) {
				this["rdf:type"] = [ new veda.IndividualModel("rdfs:Resource") ];
				return;
			}
			// Prefetch linked object (depth 2) to reduce 'get_individual' requests count during rendering
			//this.prefetch(2);
			veda.trigger("individual:loaded", this, container, template, mode);
		}
		return this;
	};

	/**
	 * @method
	 * Serialize to JSON
	 * @return {Object} JSON representation of individual.
	 */
	proto.toJson = function () {
		return this.properties;
	};

	/**
	 * @method
	 * Serialize to JSON string
	 * @return {String} JSON representation of individual.
	 */
	proto.toString = function () {
		return this["rdfs:label"] ? this["rdfs:label"].join(", ") : this.id;
	};

	/**
	 * @method
	 * Prefetch linked objects. Useful for presenting objects with many links.
	 * @param {Number} Depth of the object tree to prefetch.
	 */
	proto.prefetch = function (depth) {
		var uris = [], data = this.properties;
		Object.keys(data).map( function (key) {
			if (key === "@") return;
			data[key].map(function (value) {
				if (value.type !== "Uri") return;
				if (!veda.cache[value.data]) {
					uris.push(value.data);
				} else if (depth !== 0) {
					uris.push(veda.cache[value.data].prefetch(0));
				}
			});
		});
		uris = unique( veda.Util.flatten(uris, false) );
		for (var i=0; i < depth && uris.length; i++) {
			var result = get_individuals.call({}, veda.ticket, uris),
				res_map = result.map(function (value) {
					var obj;
					if (!veda.cache[ value["@"] ]) {
						obj = new veda.IndividualModel(value);
					} else {
						obj = veda.cache[ value["@"] ];
					}
					return obj.prefetch(0);
				});
			uris = unique( veda.Util.flatten(res_map, false) );
		}
		return uris;
	};

	function unique(arr) {
		var n = {}, r=[];
		for(var i = 0; i < arr.length; i++) {
			if (!n[arr[i]]) {
				n[arr[i]] = true;
				r.push(arr[i]);
			}
		}
		return r;
	}

});
