// Drafts Model

veda.Module(function (veda) { "use strict";

  var storage = typeof localStorage !== "undefined" ? localStorage : {
    clear: function () {
      var self = this;
      Object.keys(this).map(function (key) {
        if (typeof self[key] !== "function") delete self[key];
      });
    }
  };

  veda.DraftsModel = function () {

    // Singleton pattern
    if (veda.DraftsModel.prototype._singletonInstance) {
      return veda.DraftsModel.prototype._singletonInstance;
    }

    var self = this;

    var data = {};

    Object.defineProperty(this, "_", {
      get: function () {
        return data;
      },
      set: function (value) {
        data = value;
        return this;
      },
      enumerable: false,
      configurable: false
    });

    try {
      self._ = JSON.parse(storage.drafts);
    } catch (e) {
      self._ = {};
    }

    Object.keys(self._).map(function (key) {
      var draft = self._[key];
      if ( draft ) {
        var individual = new veda.IndividualModel( draft );
        self.set(individual.id, individual);
      }
    });

    return ( veda.DraftsModel.prototype._singletonInstance = self );
  };

  var proto = veda.DraftsModel.prototype;

  proto.has = function (uri) {
    var keys = Object.keys(this._);
    return typeof uri !== "undefined" ? !!this._[uri] : keys;
  };

  proto.get = function (uri) {
    var self = this;
    if (typeof uri !== "undefined") {
      return this[uri];
    } else {
      return Object.keys(this).map(function (uri) {
        return self[uri];
      });
    }
  };

  proto.set = function (uri, individual) {
    this[uri] = individual;
    if (!individual.hasValue("v-s:created")) individual["v-s:created"] = [ new Date() ];
    individual.isSync(false);
    this._[uri] = individual.toJson();
    storage.drafts = JSON.stringify(this._);
    veda.trigger("update:drafts", this);
    return this;
  };

  proto.reset = function (uri) {
    if ( typeof this[uri] === "object" ) {
      var individual = this.get(uri);
      individual.reset();
      delete this[uri];
      delete this._[uri];
      storage.drafts = JSON.stringify(this._);
      veda.trigger("update:drafts", this);
    }
    return this;
  };

  proto.remove = function (uri) {
    if ( typeof this[uri] === "object" ) {
      var individual = this.get(uri);
      individual.isSync(true);
      delete this[uri];
      delete this._[uri];
      storage.drafts = JSON.stringify(this._);
      veda.trigger("update:drafts", this);
    }
    return this;
  };

  proto.clear = function () {
    var self = this;
    Object.keys(this).map(function (uri) {
      if ( typeof self[uri] === "object" ) {
        var individual = self.get(uri);
        individual.reset();
        delete self[uri];
        delete self._[uri];
      }
    });
    storage.drafts = JSON.stringify(this._);
    veda.trigger("update:drafts", this);
    return this;
  };

  Object.defineProperty(proto, "length", {
    get: function () {
      return Object.keys(this._).length;
    },
    configurable: false,
    enumerable: false
  });

});
