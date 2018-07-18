/**
 * @class veda.IndividualModel
 *
 * This class is used to manipulate individuals.
 */
veda.Module(function (veda) { "use strict";

  /**
   * @constructor
   * @param {String} uri URI of individual. If not specified, than id of individual will be generated automatically.
   * @param {boolean} cache Use cache true / false. If true or not set, then object will be return from application cache (veda.cache). If false or individual not found in application cache - than individual will be loaded from database
   * @param {boolean} init individual with class model at load. If true or not set, then individual will be initialized with class specific model upon load.
   */
  veda.IndividualModel = function (uri, cache, init) {
    // veda.IndividualModel({...})
    if (typeof uri === "object" && !uri["@"]) {
      cache = uri.cache;
      init  = uri.init;
      uri   = uri.uri;
    }

    // Define Model data
    this._ = {
      cache: typeof cache !== "undefined" ? cache : true,
      init: typeof init !== "undefined" ? init : true,
      isNew: typeof uri === "undefined",
      isSync: typeof uri === "object",
      isLoaded: typeof uri === "object",
      uri: uri
    };
    this.properties = {};
    this.filtered = {};

    if (this._.cache) {
      var cached;
      if (typeof uri === "string") {
        this.id = uri;
        cached = veda.cache[ this.id ];
      } else if (typeof uri === "object") {
        this.properties = uri;
        cached = veda.cache[ this.id ];
      } else if (typeof uri === "undefined") {
        this.id = veda.Util.genUri();
      }
      if (cached) {
        return cached;
      } else {
        veda.cache[ this.id ] = this;
      }
    }

    var self = riot.observable(this);

    this.on("rdf:type", this.init);
    this.on("beforeSave", beforeSaveHandler);

    return self;
  };

  function beforeSaveHandler() {
    var now = new Date();
    var user = veda.appointment ? veda.appointment : veda.user;
    if (
      !this.hasValue("v-s:lastEditor")
      || !this.hasValue("v-s:edited")
      || this.get("v-s:lastEditor")[0].id !== user.id
      || (now - this.get("v-s:edited")[0]) > 1000
    ) {
      this.set("v-s:edited", [now]);
      this.set("v-s:lastEditor", [user]);
    }
    if ( !this.hasValue("v-s:creator") && !this.hasValue("v-s:created") ) {
      this.set("v-s:creator", [user]);
      this.set("v-s:created", [now]);
    }
  }

  var proto = veda.IndividualModel.prototype;

  proto.get = function (property_uri) {
    var self = this;
    if (!self.properties[property_uri]) return [];
    self.filtered[property_uri] = [];
    return self.properties[property_uri]
      .filter(function (value) {
        var condition = !value.lang || value.lang === "NONE" || ( veda.user && veda.user.language && value.lang in veda.user.language ) ;
        return condition ? condition : ( self.filtered[property_uri].push(value), condition );
      })
      .map( parser );
  };

  proto.set = function (property_uri, values) {
    this.isSync(false);
    values = values.filter(function (i) { return i != undefined; });
    var serialized = values.map( serializer );
    if (this.filtered[property_uri] && this.filtered[property_uri].length) {
      serialized = serialized.concat( this.filtered[property_uri] );
    }
    var uniq = unique(serialized);
    if ( JSON.stringify(this.properties[property_uri]) !== JSON.stringify(uniq) ) {
      this.properties[property_uri] = uniq;
      this.trigger("propertyModified", property_uri, values);
      this.trigger(property_uri, values);
    }
    return this;
  };

  function unique (arr) {
    var n = {}, r = [];
    for(var i = 0, val; i < arr.length; i++) {
      val = arr[i].type + arr[i].data + (arr[i].lang || "");
      if (!n[val]) {
        n[val] = true;
        r.push(arr[i]);
      }
    }
    return r;
  }

  // Define properties from ontology in veda.IndividualModel.prototype
  veda.IndividualModel.defineProperty = function (property_uri) {
    Object.defineProperty(proto, property_uri, {
      get: function () {
        return this.get(property_uri);
      },
      set: function (values) {
        return this.set(property_uri, values);
      },
      configurable: false,
      enumerable: false
    });
  };

  function parser(value) {
    if (value.type === "String") {
      var string = new String(value.data);
      if (value.lang !== "NONE") { string.language = value.lang; }
      return string;
    } else if (value.type === "Uri") {
      return new veda.IndividualModel(value.data);
    } else if (value.type === "Datetime") {
      return new Date(Date.parse(value.data));
    } else if (value.type === "Decimal") {
      return parseFloat(value.data);
    } else {
      return value.data;
    }
  }

  function serializer (value) {
    if (typeof value === "number" ) {
      return {
        type: isInteger(value) ? "Integer" : "Decimal",
        data: value
      };
    } else if (typeof value === "boolean") {
      return {
        type: "Boolean",
        data: value
      };
    } else if (typeof value === "string" || value instanceof String) {
      return {
        type: "String",
        data: value.valueOf(),
        lang: value.language || "NONE"
      };
    } else if (value instanceof Date) {
      return {
        type: "Datetime",
        data: value.toISOString()
      };
    } else if (value instanceof veda.IndividualModel) {
      return {
        type: "Uri",
        data: value.id
      };
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
      var previous = this.properties && this.properties["@"];
      if (previous && this._.cache && veda.cache[previous]) {
        delete veda.cache[previous];
        veda.cache[value] = this;
      }
      this.properties["@"] = value;
    }
  });

  Object.defineProperty(proto, "membership", {
    get: function () {
      var self = this;
      if (this._.membership) { return Promise.resolve(this._.membership); }
      if (this.isNew()) {
        this._.membership = new veda.IndividualModel({ cache: false });
        return Promise.resolve(this._.membership);
      }
      return veda.Backend.get_membership(veda.ticket, this.id).then(function (membershipJSON) {
        self._.membership = new veda.IndividualModel({ uri: membershipJSON, cache: false });
        return self._.membership.load();
      }).catch(function  (error) {
        console.log("membership error", self.id, error);
        self._.membership = new veda.IndividualModel({ cache: false });
        return self._.membership.load();
      });
    },
    configurable: false,
    enumerable: false
  });

  Object.defineProperty(proto, "rights", {
    get: function () {
      var self = this;
      if (this._.rights) { return Promise.resolve(this._.rights); }
      if (this.isNew()) {
        this._.rights = new veda.IndividualModel({ cache: false });
        this._.rights["v-s:canRead"] = [ true ];
        this._.rights["v-s:canUpdate"] = [ true ];
        this._.rights["v-s:canDelete"] = [ true ];
        return Promise.resolve(this._.rights);
      }
      return veda.Backend.get_rights(veda.ticket, this.id).then(function (rightsJSON) {
        self._.rights = new veda.IndividualModel( rightsJSON, false );
        return self._.rights.load();
      }).catch(function  (error) {
        console.log("rights error", self.id, error);
        self._.rights = new veda.IndividualModel({ cache: false });
        return self._.rights.load();
      });
    },
    configurable: false,
    enumerable: false
  });

  Object.defineProperty(proto, "rightsOrigin", {
    get: function () {
      var self = this;
      if (this._.rightsOrigin) { return Promise.resolve(this._.rightsOrigin); }
      return veda.Backend.get_rights_origin(veda.ticket, this.id).then(function (rightsOriginArr) {
        self._.rightsOrigin = rightsOriginArr.map(function (item) {
          return new veda.IndividualModel( item, false ).load();
        });
        return Promise.all(self._.rightsOrigin);
      }).catch(function  (error) {
        console.log("rights error", self.id, error);
        self._.rightsOrigin = [];
        return Promise.resolve(self._.rightsOrigin);
      });
    },
    configurable: false,
    enumerable: false
  });

  /**
   * @method
   * Load individual specified by uri from database. If cache parameter (from constructor) is true, than try to load individual from browser cache first.
   * @param {String} uri individual uri
   */
  proto.load = function () {
    var self = this;
    var uri = this._.uri ;
    this.trigger("beforeLoad");
    if (typeof uri === "string") {
      if ( this.isLoaded() ) {
        this.trigger("afterLoad", this);
        return Promise.resolve( this );
      }
      return veda.Backend.get_individual(veda.ticket, uri).then(function (individualJson) {
        self.isNew(false);
        self.isSync(true);
        self.isLoaded(true);
        self.properties = individualJson;
        self.trigger("afterLoad", self);
        if (self._.init) {
          return self.init();
        }
        return self;
      }).catch(function (error) {
        console.log("load individual error", self.id, error);
        if (error.code === 422 || error.code === 404) {
          self.isNew(true);
          self.isSync(false);
          self.isLoaded(false);
          self.properties = {
            "@": uri,
            "rdf:type": [{type: "Uri", data: "rdfs:Resource"}],
            "rdfs:label": [
              {type: "String", data: "Объект не существует", lang: "RU"},
              {type: "String", data: "Object does not exist", lang: "EN"}
            ]
          };
        } else if (error.code === 472) {
          self.isNew(false);
          self.isSync(false);
          self.isLoaded(false);
          self.properties = {
            "@": uri,
            "rdf:type": [{type: "Uri", data: "rdfs:Resource"}],
            "rdfs:label": [
              {type: "String", data: "Нет прав на объект", lang: "RU"},
              {type: "String", data: "Insufficient rights", lang: "EN"}
            ]
          };
        } else if (error.code === 470 || error.code === 471) {
          self.isNew(false);
          self.isSync(false);
          self.isLoaded(false);
        } else {
          self.isNew(false);
          self.isSync(false);
          self.isLoaded(false);
          self.properties = {
            "@": uri,
            "rdf:type": [{type: "Uri", data: "rdfs:Resource"}],
            "rdfs:label": [{type: "String", data: uri, lang: "NONE"}]
          };
        }
        self.trigger("afterLoad", self);
        return self;
      });

    } else if (typeof uri === "object") {
      this.isNew(false);
      this.isSync(true);
      this.isLoaded(true);
      this.properties = uri;
    } else if (typeof uri === "undefined") {
      this.isNew(true);
      this.isSync(false);
      this.isLoaded(false);
    }
    this.trigger("afterLoad", this);
    if (this._.init) {
      return this.init();
    }
    return Promise.resolve(this);
  };

  /**
   * @method
   * Save current individual to database
   */
  proto.save = function() {
    var self = this;
    this.trigger("beforeSave");
    // Do not save individual to server if nothing changed
    if (this.isSync()) {
      this.trigger("afterSave");
      return Promise.resolve(this);
    }
    Object.keys(this.properties).reduce(function (acc, property_uri) {
      if (property_uri === "@") return acc;
      acc[property_uri] = acc[property_uri].filter(function (item) {
        return item && item.data !== "";
      });
      if (!acc[property_uri].length) delete acc[property_uri];
      return acc;
    }, this.properties);

    return veda.Backend.put_individual(veda.ticket, this.properties).then(function () {
      self.isNew(false);
      self.isSync(true);
      self.trigger("afterSave");
      return self;
    });
  }

  /**
   * @method
   * Reset current individual to database
   */
  proto.reset = function () {
    var self = this;
    this.trigger("beforeReset");
    self.filtered = {};
    return veda.Backend.get_individual(veda.ticket, self.id).then(function (original) {
      var self_property_uris = Object.keys(self.properties);
      var original_property_uris = Object.keys(original);
      var union = veda.Util.unique( self_property_uris.concat(original_property_uris) );
      union.forEach(function (property_uri) {
        var modified = false;
        if (property_uri === "@") { return; }
        if (!self.properties[property_uri]) {
          self.properties[property_uri] = original[property_uri];
          modified = true;
        } else if (!original[property_uri]) {
          delete self.properties[property_uri];
          modified = true;
        } else {
          var currentSum = JSON.stringify(self.properties[property_uri]).split("").reduce(function (acc, char) {return acc += char.charCodeAt(0);}, 0);
          var originalSum = JSON.stringify(original[property_uri]).split("").reduce(function (acc, char) {return acc += char.charCodeAt(0);}, 0);
          if (currentSum !== originalSum) {
            self.properties[property_uri] = original[property_uri];
            modified = true;
          }
        }
        if (modified) {
          self.trigger("propertyModified", property_uri, self.get(property_uri));
          self.trigger(property_uri, self.get(property_uri));
        }
      });
      self.isNew(false);
      self.isSync(true);
      self.trigger("afterReset");
      return self;
    }).catch(function (error) {
      console.log("reset individual error", error);
      self.trigger("afterReset");
    });
  };

  /**
   * @method
   * Mark current individual as deleted in database (add v-s:deleted property)
   */
  proto.delete = function () {
    this.trigger("beforeDelete");
    if ( this.isNew() ) {
      this.trigger("afterDelete");
      return Promise.resolve(this);
    }
    this["v-s:deleted"] = [ true ];
    this.trigger("afterDelete");
    return this.save();
  };

  /**
   * @method
   * Remove individual from database
   */
  proto.remove = function () {
    var self = this;
    this.trigger("beforeRemove");
    if ( this._.cache && veda.cache && veda.cache[this.id] ) {
      delete veda.cache[this.id];
    }
    if ( this.isNew() ) {
      this.trigger("afterRemove");
      return Promise.resolve(this);
    }
    return veda.Backend.remove_individual(veda.ticket, this.id).then(function () {
      self.trigger("afterRemove");
      return self;
    });
  };

  /**
   * @method
   * Recover current individual in database (remove v-s:deleted property)
   */
  proto.recover = function () {
    this.trigger("beforeRecover");
    this["v-s:deleted"] = [];
    this.trigger("afterRecover");
    return this.save();
  };

  /**
   * @method
   * @param {String} property_uri property name
   * @return {boolean} is requested property exists in this individual
   */
  proto.hasValue = function (property_uri, value) {
    var result = !!(this.properties[property_uri] && this.properties[property_uri].length);
    if (typeof value !== "undefined" && value !== null) {
      var serialized = serializer(value);
      result = result && !!this.properties[property_uri].filter( function (item) {
        return ( item.data == serialized.data && (item.lang && serialized.lang ? item.lang === serialized.lang : true) );
      }).length;
    }
    return result;
  };

  /**
   * @method
   * @param {String} property_uri property name
   * @param {Any allowed type} value
   * @return {this}
   */
  proto.addValue = function (property_uri, values) {
    if (typeof values === "undefined" || values === null) {
      return this;
    }
    this.properties[property_uri] = this.properties[property_uri] || [];
    if ( Array.isArray(values) ) {
      var that = this;
      values.forEach(function (value) {
        addSingleValue.call(that, property_uri, value);
      });
    } else {
      addSingleValue.call(this, property_uri, values);
    }
    this.isSync(false);
    values = this.get(property_uri);
    this.trigger("propertyModified", property_uri, values);
    this.trigger(property_uri, values);
    return this;
  };
  function addSingleValue(property_uri, value) {
    if (value != undefined) {
      var serialized = serializer(value);
      this.properties[property_uri].push(serialized);
    }
  }

  /**
   * @method
   * @param {String} property_uri property name
   * @param {Any allowed type} value
   * @return {this}
   */
  proto.removeValue = function (property_uri, values) {
    if (!this.properties[property_uri] || !this.properties[property_uri].length || typeof values === "undefined" || values === null) {
      return this;
    }
    if ( Array.isArray(values) ) {
      var that = this;
      values.forEach(function (value) {
        removeSingleValue.call(that, property_uri, value);
      });
    } else {
      removeSingleValue.call(this, property_uri, values);
    }
    this.isSync(false);
    values = this.get(property_uri);
    this.trigger("propertyModified", property_uri, values);
    this.trigger(property_uri, values);
    return this;
  };
  function removeSingleValue (property_uri, value) {
    if (value != undefined) {
      var serialized = serializer(value);
      this.properties[property_uri] = (this.properties[property_uri] || []).filter(function (item) {
        return !( item.data == serialized.data && (item.lang && serialized.lang ? item.lang === serialized.lang : true) );
      });
    }
  }

  /**
   * @method
   * @param {String} property_uri property name
   * @param {Any allowed type} value
   * @return {this}
   */
  proto.toggleValue = function (property_uri, values) {
    if (typeof values === "undefined" || values === null) {
      return this;
    }
    this.properties[property_uri] = this.properties[property_uri] || [];
    if ( Array.isArray(values) ) {
      var that = this;
      values.forEach(function (value) {
        toggleSingleValue.call(that, property_uri, value);
      });
    } else {
      toggleSingleValue.call(this, property_uri, values);
    }
    this.isSync(false);
    values = this.get(property_uri);
    this.trigger("propertyModified", property_uri, values);
    this.trigger(property_uri, values);
    return this;
  };
  function toggleSingleValue (property_uri, value) {
    if (value != undefined) {
      if ( this.hasValue(property_uri, value) ) {
        removeSingleValue.call(this, property_uri, value);
      } else {
        addSingleValue.call(this, property_uri, value);
      }
    }
  }

  /**
   * @method
   * @param {String} property_uri property name
   * @return {this}
   */
  proto.clearValue = function (property_uri) {
    if (!this.properties[property_uri] || !this.properties[property_uri].length) {
      return this;
    } else {
      var empty = [];
      this.properties[property_uri] = empty;
      this.isSync(false);
      this.trigger("propertyModified", empty);
      this.trigger(property_uri, empty);
    }
    return this;
  };

  /**
   * @method
   * @param {String} id of class to check
   * @return {boolean} is individual rdf:type subclass of requested class
   */
  proto.is = function (_class) {
    var self = this;
    if (typeof _class.valueOf() === "string") {
      _class = new veda.IndividualModel( _class.valueOf() );
    }
    var types = self.get("rdf:type");
    var is = eval(
      types.map(function (type) {
        return self.hasValue("rdf:type", _class.id);
      }).join("||")
    );
    if (is) {
      return Promise.resolve(is);
    } else {
      return Promise.all(types.map(isSub)).then(function (results) {
        return eval(results.join("||"));
      });
    }

    function isSub(type) {
      return type.load().then(function (type) {
        if (is) { return is; }
        if (!type.hasValue("rdfs:subClassOf")) {
          return (is = is || false);
        } else if (type.hasValue("rdfs:subClassOf", _class.id)) {
          return (is = is || true);
        } else {
          var types = type.get("rdfs:subClassOf");
          return Promise.all(types.map(isSub)).then(function (results) {
            return eval(results.join("||"));
          });
        }
      });
    }
  };

  /**
   * @method
   * Initialize individual with class specific domain properties and methods
   */
  proto.init = function () {
    var self = this;
    var isClass = this.hasValue("rdf:type", "owl:Class") || this.hasValue("rdf:type", "rdfs:Class");
    if ( this.hasValue("v-ui:hasModel") && !isClass ) {
      return this.get("v-ui:hasModel")[0].load()
        .then(function (model) {
          if ( !model.modelFn ) {
            model.modelFn = new Function(model["v-s:script"][0]);
          }
          model.modelFn.call(self);
          return self;
        });
    } else {
      var types_promises = this.get("rdf:type").map( function (type_promise) {
        return type_promise.load();
      });
      return Promise.all( types_promises )
        .then( function (types) {
          var models_promises = [];
          types.map( function (type) {
            if ( type.hasValue("v-ui:hasModel") ) {
              models_promises.push( type.get("v-ui:hasModel")[0].load() );
            }
          });
          return Promise.all( models_promises );
        })
        .then( function (models) {
          models.map(function (model) {
            if ( !model.modelFn ) {
              model.modelFn = new Function(model["v-s:script"][0]);
            }
            model.modelFn.call(self);
          });
          return self;
        });
    }
  };

  /**
   * @method
   * Clone individual with different (generated) id
   * @return {veda.IndividualModel} clone of this individual with different id.
   */
  proto.clone = function () {
    var properties = JSON.parse( JSON.stringify(this.properties) );
    properties["@"] = veda.Util.genUri();
    var clone = new veda.IndividualModel(properties);
    clone.isNew(true);
    clone.isSync(false);
    return clone.init();
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
   * Check whether individual was loaded from db
   * @return {boolean}
   */
  proto.isLoaded = function (value) {
    return ( typeof value !== "undefined" ? this._.isLoaded = value : this._.isLoaded );
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
    if ( this.hasValue("rdfs:label") ) {
      return this.get("rdfs:label").join(" ");
    } else if ( this.hasValue("rdf:type") ) {
      return this.get("rdf:type")[0].get("rdfs:label").join(" ");
    } else {
      return this.id;
    }
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
    uris = veda.Util.unique( veda.Util.flatten(uris, false) );
    for (var i = 0; i < depth && uris.length; i++) {
      var result = veda.Backend.get_individuals(veda.ticket, uris),
        res_map = result.map(function (value) {
          var obj;
          if (!veda.cache[ value["@"] ]) {
            obj = new veda.IndividualModel(value);
          } else {
            obj = veda.cache[ value["@"] ];
          }
          return prefetch.apply( obj, [0].concat(allowed_props) );
        });
      uris = veda.Util.unique( veda.Util.flatten(res_map, false) );
    }
    return uris;
  };

});
