/**
 * @class veda.IndividualModel
 *
 * This class is used to manipulate individuals.
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

    // veda.IndividualModel({...})
    if (typeof uri === "object" && !uri["@"]) {
      container = uri.container;
      template  = uri.template;
      mode      = uri.mode;
      cache     = uri.cache;
      init      = uri.init;
      uri       = uri.uri;
    }

    // Define Model functions
    this._ = {};
    this._.cache = typeof cache !== "undefined" ? cache : true;
    this._.init = typeof init !== "undefined" ? init : true;
    this._.isNew = false;
    this._.isSync = false;
    this.properties = {};
    this._.filtered = {};

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
      if (
        this.hasValue("v-s:lastEditor")
        && this.hasValue("v-s:edited")
        && this["v-s:lastEditor"][0].id === editor.id
        && (now - this["v-s:edited"][0]) < 1000
      ) { return; }
      this["v-s:edited"] = [ now ];
      this["v-s:lastEditor"] = [ editor ];
      if (!this.hasValue("v-s:created")) this["v-s:created"] = [ now ];
      if (!this.hasValue("v-s:creator")) this["v-s:creator"] = [ editor ];
    });

    if (container) {
      this.one("individual:afterLoad", function (individual) {
        this.present.call(individual, container, template, mode);
        container = template = mode = null;
      });
      /*this.on("individual:typeChanged", function () {
        this.present(container, template, mode);
      });*/
    }

    /*veda.on("language:changed", function () {
      self._.filtered = {};
    });*/

    return self.load(uri);
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
        var notNull = values.filter(function (i) { return i != undefined });
        var serialized = notNull.map( serializer );
        if (this._.filtered[property_uri] && this._.filtered[property_uri].length) {
          serialized = serialized.concat( this._.filtered[property_uri] );
        }
        if ( JSON.stringify(this.properties[property_uri]) !== JSON.stringify(serialized) ) {
          this.properties[property_uri] = serialized;
          this.trigger("individual:propertyModified", property_uri, notNull);
        }
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
      this.properties["@"] = value;
      this.trigger("individual:idChanged", value);
    }
  });

  Object.defineProperty(proto, "rights", {
    get: function () {
      if (this._.rights) return this._.rights;
      if (this._.isNew || this.hasValue("v-s:isDraft", true)) {
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
    this.trigger("individual:beforeLoad");
    if (typeof uri === "string") {
      this.id = uri;
      if (this._.cache && veda.cache[uri]) {
        this.trigger("individual:afterLoad", veda.cache[uri]);
        return veda.cache[uri];
      }
      try {
        this._.isNew = false;
        this._.isSync = true;
        this.properties = get_individual(veda.ticket, uri);
      } catch (e) {
        if (e.status === 422) {
          this._.isNew = true;
          this._.isSync = false;
          this.properties = {
            "@": uri,
            "rdfs:label": [{type: "String", data: uri, lang: "NONE"}],
            "rdf:type": [{type: "Uri", data: "rdfs:Resource"}]
          };
        } else if (e.status === 472) {
          this._.isNew = false;
          this._.isSync = false;
          this.properties = {
            "@": uri,
            "rdfs:label": [
              {type: "String", data: "No rights", lang: "EN"},
              {type: "String", data: "Нет прав", lang: "RU"}
            ],
            "rdf:type": [{type: "Uri", data: "rdfs:Resource"}]
          };
        } else {
          this._.isNew = false;
          this._.isSync = false;
          this.properties = {
            "@": uri,
            "rdfs:label": [{type: "String", data: uri, lang: "NONE"}],
            "rdf:type": [{type: "Uri", data: "rdfs:Resource"}]
          };
        }
      }
    } else if (typeof uri === "object") {
      this._.isNew = false;
      this._.isSync = true;
      this.properties = uri;
    } else if (typeof uri === "undefined") {
      this._.isNew = true;
      this._.isSync = false;
      this.id = veda.Util.genUri();
    }
    if (this._.cache) veda.cache[this.id] = this;
    if (this._.init) this.init();
    this.trigger("individual:afterLoad", this);
    return this;
  };

  /**
   * @method
   * Save current individual to database (with validation and adding new version)
   */
  proto.save = function() {
    var self = this;
    // Do not save individual to server if nothing changed
    if (self._.isSync) return;
    self.trigger("individual:beforeSave");
    if ( this.hasValue("v-s:isDraft", true) ) {
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
      if (e.status !== 472) {
        this.draft();
      } else {
        console.log("Нет прав на создание или изменение объекта / No rights to create or modify object\n" + this.id + " (" + this.toString() + ")");
      }
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
  proto.draft = function() {
    this.trigger("individual:beforeDraft");
    veda.drafts.set(this.id, this);
    this.trigger("individual:afterDraft");
    return this;
  }

  /**
   * @method
   * Reset current individual to database
   */
  proto.reset = function () {
    this.trigger("individual:beforeReset");
    this.update();
    this.trigger("individual:afterReset");
    return this;
  };

  /**
   * @method
   * Update current individual with values from database & merge with local changes
   */
  proto.update = function () {
    this.trigger("individual:beforeUpdate");
    var self = this;
    if (!this._.isNew) {
      this._.filtered = {};
      var original;
      try {
        original = get_individual(veda.ticket, this.id);
      } catch (e) {
        original = {};
      }
      Object.keys(self.properties).map(function (property_uri) {
        if (property_uri === "@") {
          delete original[property_uri];
          return;
        }
        if (original[property_uri] && original[property_uri].length) {
          self[property_uri] = original[property_uri].map( parser );
        } else {
          self[property_uri] = [];
        }
        delete original[property_uri];
      });
      Object.keys(original).map(function (property_uri) {
        self[property_uri] = original[property_uri].map( parser );
      });
      self._.isNew = false;
      self._.isSync = true;
    }
    veda.drafts.remove(this.id);
    this.trigger("individual:afterUpdate");
    return this;
  };

  /**
   * @method
   * Mark current individual as deleted in database (add v-s:deleted property)
   */
  proto.delete = function () {
    this.trigger("individual:beforeDelete");
    if ( this.hasValue("v-s:isDraft", true) ) {
      veda.drafts.remove(this.id);
    }
    if ( !this.isNew() ) {
      this["v-s:deleted"] = [ true ];
      this.save();
    }
    this.trigger("individual:afterDelete");
    return this;
  };

  /**
   * @method
   * Recover current individual in database (remove v-s:deleted property)
   */
  proto.recover = function () {
    this.trigger("individual:beforeRecover");
    if ( this.hasValue("v-s:isDraft", true) ) {
      veda.drafts.remove(this.id);
    }
    this["v-s:deleted"] = [];
    this.save();
    this.trigger("individual:afterRecover");
    return this;
  };

  /**
   * @method
   * @param {String} property_uri property name
   * @return {boolean} is requested property exists in this individual
   */
  proto.hasValue = function (property_uri, value) {
    var result = !!(this.properties[property_uri] && this.properties[property_uri].length);
    if (typeof value !== "undefined") {
      var serialized = serializer(value);
      result = result && !!this.properties[property_uri].filter( function (item) {
        return ( item.data === serialized.data && item.type === serialized.type && (item.lang && serialized.lang ? item.lang === serialized.lang : true) );
      }).length;
    }
    return result;
  };

  /**
   * @method
   * @param {String} id of class to check
   * @return {boolean} is individual rdf:type subclass of requested class
   */
  proto.is = function (_class) {
    if (typeof _class.valueOf() === "string") {
      _class = new veda.IndividualModel( _class.valueOf() );
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
        var model = new Function(_class.model["v-s:script"][0]);
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
  proto.clone = function () {
    var individual = JSON.parse( JSON.stringify(this.properties) );
    individual["@"] = veda.Util.genUri();
    var clone = new veda.IndividualModel(individual);
    clone.isNew(true);
    return clone;
  };

  /**
   * @method
   * Check whether individual is synchronized with db
   * @return {boolean}
   */
  proto.isSync = function (value) {
    return ( typeof value !== "undefined" ? this._.isSync = value : this._.isSync );
  };

  /**
   * @method
   * Check whether individual is new (not saved in db)
   * @return {boolean}
   */
  proto.isNew = function (value) {
    return ( typeof value !== "undefined" ? this._.isNew = value : this._.isNew );
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
   * Serialize to string
   * @return {String} String representation of individual.
   */
  proto.toString = function () {
    //return this["rdf:type"][0]["rdfs:label"].join(", ") + ": " + ( this["rdfs:label"] ? this["rdfs:label"].join(", ") : this.id );
    return this.hasValue("rdfs:label") ? this["rdfs:label"].join(" ") : this["rdf:type"][0]["rdfs:label"].join(" ") + ": " + this.id ;
  };

  /**
   * @method
   * Return self
   * @return {Object} self.
   */
  proto.valueOf = function () {
    return this;
  };

  /**
   * @method
   * Prefetch linked objects. Useful for presenting objects with many links.
   * @param {Number} Depth of the object tree to prefetch.
   * @param {allowed_property_uri, ...} Allowed property uri for links. If defined the tree is formed only for allowed properties.
   */
  proto.prefetch = function (depth) {
    var allowed_props = [].slice.call(arguments, 1),
        uris = [],
        data = this.properties,
        prefetch = this.prefetch;
    Object.keys(data).map( function (key) {
      if ( key === "@" || (allowed_props.length && allowed_props.indexOf(key) < 0) ) return;
      data[key].map(function (value) {
        if (value.type !== "Uri") return;
        if (!veda.cache[value.data]) {
          uris.push(value.data);
        } else if (depth !== 0) {
          uris.push( prefetch.apply( veda.cache[value.data], [0].concat(allowed_props) ) );
        }
      });
    });
    uris = unique( veda.Util.flatten(uris, false) );
    for (var i = 0; i < depth && uris.length; i++) {
      var result = get_individuals(veda.ticket, uris),
        res_map = result.map(function (value) {
          var obj;
          if (!veda.cache[ value["@"] ]) {
            obj = new veda.IndividualModel(value);
          } else {
            obj = veda.cache[ value["@"] ];
          }
          return prefetch.apply( obj, [0].concat(allowed_props) );
        });
      uris = unique( veda.Util.flatten(res_map, false) );
    }
    return uris;
  };

  function unique(arr) {
    var n = {}, r = [];
    for(var i = 0; i < arr.length; i++) {
      if (!n[arr[i]]) {
        n[arr[i]] = true;
        r.push(arr[i]);
      }
    }
    return r;
  }

});
