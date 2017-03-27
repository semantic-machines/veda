/**
 * @class veda.IndividualModelAsync
 *
 * This class is used to manipulate individuals (async).
 */
veda.Module(function (veda) { "use strict";

  /**
   * @constructor
   * @param {String} uri URI of individual. If not specified, than id of individual will be generated automatically.
   * @param {boolean} cache Use cache true / false. If true or not set, then object will be return from application cache (veda.cache). If false or individual not found in application cache - than individual will be loaded from database
   * @param {boolean} init individual with class model at load. If true or not set, then individual will be initialized with class specific model upon load.
   */
  veda.IndividualModelAsync = function (uri, container, template, mode, cache, init) {

    var self = riot.observable(this);

    // veda.IndividualModelAsync({...})
    if (typeof uri === "object" && !uri["@"]) {
      container = uri.container;
      template  = uri.template;
      mode      = uri.mode;
      cache     = uri.cache;
      init      = uri.init;
      uri       = uri.uri;
    }

    // Define Model functions
    this._ = {
      cache: typeof cache !== "undefined" ? cache : true,
      init: typeof init !== "undefined" ? init : true,
      isNew: typeof uri === "undefined",
      isSync: false,
      uri: uri
    };

    if ( typeof uri === "string" ) {
      this.properties = {"@": uri};
    } else if ( typeof uri === "object" && uri["@"] ) {
      this.properties = uri;
    } else {
      this.properties = {};
    }
    this.filtered = {};

    this.on("individual:propertyModified", typeChangedHandler);
    this.on("individual:beforeSave", beforeSaveHandler);

    return this;
  };

  function typeChangedHandler (property_uri) {
    if (property_uri === "rdf:type") {
      this.init();
    }
  }

  function beforeSaveHandler() {
    var now = new Date();
    var user = veda.appointment ? veda.appointment : veda.user;
    if (
      !this.hasValue("v-s:lastEditor")
      || !this.hasValue("v-s:edited")
      || this["v-s:lastEditor"][0].id !== user.id
      || (now - this["v-s:edited"][0]) > 1000
    ) {
      this["v-s:edited"] = [ now ];
      this["v-s:lastEditor"] = [ user ];
    }
    if ( this.isNew() ) {
      this["v-s:created"] = [ now ];
      this["v-s:creator"] = [ user ];
    }
  }

  var proto = veda.IndividualModelAsync.prototype;

  // Define properties from ontology in veda.IndividualModelAsync.prototype
  veda.IndividualModelAsync.defineProperty = function (property_uri) {
    Object.defineProperty(proto, property_uri, {
      get: function () {
        var self = this;
        if (!self.properties[property_uri]) return [];
        var values = self.properties[property_uri]
          .filter(function (value) {
            var condition = value.type !== "String" || value.lang === "NONE" || (veda.user && veda.user.language && value.lang in veda.user.language);
            if (condition === false) {
              var filtered = self.filtered[property_uri] || [],
                  found = filtered.filter(function (filteredVal) {
                    return filteredVal.data === value.data && filteredVal.lang === value.lang;
                  });
              if ( !found.length ) {
                filtered.push( value );
              }
              self.filtered[property_uri] = filtered;
            }
            return condition;
          })
          .map( parser );
        return values;
      },
      set: function (values) {
        var self = this;
        this.isSync(false);
        var notNull = values.filter(function (i) { return i != undefined });
        var serialized = notNull.map( serializer );
        if (this.filtered[property_uri] && this.filtered[property_uri].length) {
          serialized = serialized.concat( this.filtered[property_uri] );
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

  function parser (value) {
    if (value.type === "String") {
      var string = new String(value.data);
      if (value.lang !== "NONE") { string.language = value.lang };
      return string;
    } else if (value.type === "Uri") {
      if (value.data.search(/^.{3,5}:\/\//) === 0) return value.data;
      return new veda.IndividualModelAsync({uri: value.data});
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
    } else if (value instanceof veda.IndividualModel || value instanceof veda.IndividualModelAsync) {
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

  Object.defineProperty(proto, "membership", {
    get: function () {
      if (this._.membership) return this._.membership;
      if (this.isNew() || this.hasValue("v-s:isDraft", true)) {
        this._.membership = new veda.IndividualModelAsync({ cache: false });
        return this._.membership;
      }
      try {
        var membershipJSON = get_membership(veda.ticket, this.id);
        this._.membership = new veda.IndividualModelAsync({ uri: membershipJSON, cache: false });
      } catch (e) {
        this._.membership = new veda.IndividualModelAsync();
      } finally {
        return this._.membership;
      }
    },
    configurable: false,
    enumerable: false
  });

  Object.defineProperty(proto, "rights", {
    get: function () {
      if (this._.rights) return this._.rights;
      if (this.isNew() || this.hasValue("v-s:isDraft", true)) {
        this._.rights = new veda.IndividualModelAsync({ cache: false });
        this._.rights["v-s:canRead"] = [ true ];
        this._.rights["v-s:canUpdate"] = [ true ];
        this._.rights["v-s:canDelete"] = [ true ];
        return this._.rights;
      }
      try {
        var rightsJSON = get_rights(veda.ticket, this.id);
        this._.rights = new veda.IndividualModelAsync( rightsJSON, undefined, undefined, undefined, false );
      } catch (e) {
        this._.rights = new veda.IndividualModelAsync();
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
          return new veda.IndividualModelAsync( item, undefined, undefined, undefined, false );
        });
      } catch (e) {
        this._.rightsOrigin = [];
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
  proto.load = function () {
    var uri = this._.uri;
    var self = this;
    this.trigger("individual:beforeLoad");
    if (typeof uri === "string") {
      this.id = uri;

      if (this._.cache && veda.cache[uri]) {
        if ( veda.cache[uri] instanceof veda.IndividualModelAsync ) {
          this.trigger("individual:afterLoad", veda.cache[uri]);
          return Promise.resolve(veda.cache[uri]);
        } else if ( veda.cache[uri] instanceof veda.IndividualModel ) {
          var asyncModel = new veda.IndividualModelAsync( veda.cache[uri].properties );
          return asyncModel.load();
        }
      }

      return get_individual({ticket: veda.ticket, uri: uri, async: true})
        .then(function (properties) {
          self.properties = properties;
          self.isNew(false);
          self.isSync(true);
          if (self._.cache) veda.cache[self.id] = self;
          if (self._.init) self.init();
          self.trigger("individual:afterLoad", self);
          return self;
        })
        .catch(function (error) {
          var notify = veda.Notify ? new veda.Notify() : function () {};
          notify("danger", error);
          if (error.code === 422) {
            this.isNew(true);
            this.isSync(false);
            this.properties = {
              "@": uri,
              "rdfs:label": [{type: "String", data: uri, lang: "NONE"}],
              "rdf:type": [{type: "Uri", data: "rdfs:Resource"}]
            };
          } else if (error.code === 472) {
            this.isNew(false);
            this.isSync(false);
            this.properties = {
              "@": uri,
              "rdfs:label": [
                {type: "String", data: "No rights", lang: "EN"},
                {type: "String", data: "Нет прав", lang: "RU"}
              ],
              "rdf:type": [{type: "Uri", data: "rdfs:Resource"}]
            };
          } else {
            this.isNew(false);
            this.isSync(false);
            this.properties = {
              "@": uri,
              "rdfs:label": [{type: "String", data: uri, lang: "NONE"}],
              "rdf:type": [{type: "Uri", data: "rdfs:Resource"}]
            };
          }
          return self;
        });
    } else if (typeof uri === "object") {
      this.isNew(false);
      this.isSync(true);
      this.properties = uri;
    } else if (typeof uri === "undefined") {
      this.isNew(true);
      this.isSync(false);
      this.id = veda.Util.genUri();
    }
    if (this._.cache) veda.cache[this.id] = this;
    if (this._.init) this.init();
    this.trigger("individual:afterLoad", this);
    return Promise.resolve(this);
  };

  /**
   * @method
   * Save current individual to database (with validation and adding new version)
   */
  proto.save = function () {
    var self = this;
    // Do not save individual to server if nothing changed
    if (self.isSync()) {
      return Promise.resolve(self);
    }
    self.trigger("individual:beforeSave");
    if ( self.hasValue("v-s:isDraft", true) ) {
      veda.drafts.remove(self.id);
    }
    Object.keys(self.properties).reduce(function (acc, property_uri) {
      if (property_uri === "@") return acc;
      acc[property_uri] = self.properties[property_uri].filter(function (item) {
        return item && item.data !== "";
      });
      if (!acc[property_uri].length) delete acc[property_uri];
      return acc;
    }, self.properties);
    return put_individual({ticket: veda.ticket, individual: self.properties, async: true})
      .then(function () {
        self.isNew(false);
        self.isSync(true);
        if (self._.cache) veda.cache[self.id] = self;
        self.trigger("individual:afterSave");
        return self;
      })
      .catch(function (error) {
        var notify = veda.Notify ? new veda.Notify() : function () {};
        notify("danger", error);
        if (error.code !== 472) {
          self.draft();
        }
        return Promise.resolve(self);
      });
  }

  /**
   * @method
   * Save current individual without validation and without adding new version
   */
  proto.draft = function () {
    this.trigger("individual:beforeDraft");
    veda.drafts.set(this);
    this.trigger("individual:afterDraft");
    return this;
  }

  /**
   * @method
   * Reset current individual to database
   */
  proto.reset = function () {
    var self = this;
    this.trigger("individual:beforeReset");
    return this.update()
      .then(function (self) {
        self.trigger("individual:afterReset");
        return self;
      });
  };

  /**
   * @method
   * Update current individual with values from database & merge with local changes
   */
  proto.update = function () {
    this.trigger("individual:beforeUpdate");
    var self = this;
    if (!this.isNew()) {
      this.filtered = {};
      return get_individual({ticket: veda.ticket, uri: this.id, async: true})
        .then(function (original) {
          merge.call(self, original);
          veda.drafts.remove(self.id);
          self.trigger("individual:afterUpdate");
          return self;
        })
        .catch(function (error) {
          var notify = veda.Notify ? new veda.Notify() : function () {};
          notify("danger", error);
          return self;
        });
    }
    veda.drafts.remove(this.id);
    this.trigger("individual:afterUpdate");
    return Promise.resolve(this);
  };

  function merge (original) {
    var self = this;
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
    self.isNew(false);
    self.isSync(true);
  }

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
      return this.save()
        .then( function (self) {
          self.trigger("individual:afterDelete");
          return self;
        });
    }
    this.trigger("individual:afterDelete");
    return Promise.resolve(this);
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
    if ( !this.isNew() ) {
      this["v-s:deleted"] = [];
      return this.save()
        .then( function (self) {
          self.trigger("individual:afterRecover");
          return self;
        });
      }
    this.trigger("individual:afterRecover");
    return Promise.resolve(this);
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
      _class = new veda.IndividualModelAsync( _class.valueOf() );
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
    var types_promises = this["rdf:type"].map( function (type_promise) {
      return type_promise.load();
    });
    return Promise.all( types_promises )
      .then( function (types) {
        var models_promises = [];
        types.map( function (type) {
          if ( type.hasValue("v-ui:hasModel") ) {
            models_promises.push( type["v-ui:hasModel"] );
          }
        });
        return Promise.all( models_promises );
      })
      .then( function (models) {
        models.map(function (model) {
          var model_fn = new Function(model["v-s:script"][0]);
          model.call(self);
        });
        return self;
      });
  };

  /**
   * @method
   * Clone individual with different (generated) id
   * @return {veda.IndividualModelAsync} clone of this individual with different id.
   */
  proto.clone = function () {
    var clone_properties = JSON.parse( JSON.stringify(this.properties) );
    clone_properties["@"] = veda.Util.genUri();
    var clone = new veda.IndividualModelAsync(clone_properties);
    clone.isNew(true);
    clone.isSync(false);
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
   * @param {String/jQuery/veda.IndividualModelAsync} template Template to render individual with.
   * @param {String} mode Initial mode for individual presenter. Expected values: "view", "edit", "search".
   */
  proto.present = function (container, template, mode) {
    if (container) {
      veda.trigger("individual_async:loaded", this, container, template, mode);
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
    return this.hasValue("rdfs:label") ? this["rdfs:label"].join(" ") : this.hasValue("rdf:type") ? this["rdf:type"][0].id + ": " + this.id : this.id ;
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
            obj = new veda.IndividualModelAsync(value);
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
