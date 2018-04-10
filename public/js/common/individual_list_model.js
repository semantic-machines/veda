// Individual list Model

veda.Module(function (veda) { "use strict";

  veda.IndividualListModel = function () {

    var self = riot.observable(this);

    function transform (arr) {
      var result = [];
      for (var i in arr) flatten (arr[i]);
      function flatten (el) {
        if (Array.isArray(el)) {
          return el.forEach(flatten);
        }
        return result.push(el);
      }
      return result;
    }

    var args = transform(arguments);

    var keys = [];

    args.map( function (item) {
      switch (true) {
        case typeof item === "string" :
          keys.push(item);
          Object.defineProperty(self, item, {
            get: function () { return new veda.IndividualModel(item); },
            configurable: true
          });
          break;
        case item instanceof veda.IndividualModel :
          keys.push(item.id);
          Object.defineProperty(self, item.id, {
            get: function () { return item; },
            configurable: true
          });
          break;
        case typeof item === "object" :
          for (var key in item) {
            keys.push(key);
            var value = item[key];
            (function (val) {
              Object.defineProperty(self, key, {
                get: function () { return val; },
                configurable: true
              });
            })(value);
          }
          break;
      }
    });

    self.each = function (callback) {
      keys.map(function (key, index) {
        callback(self[key], key, index);
      });
      return self;
    };

    self.map = function (callback) {
      return keys.map(function (key, index) {
        return callback(self[key], key, index);
      });
    };

    self.filter = function (callback) {
      var result = [];
      keys.map(function (key, index) {
        if ( callback(self[key], key, index) ) {
          result.push( self[key] );
        }
      });
      return result;
    };

    self.add = function (individual) {
      keys.push(individual.id);
      Object.defineProperty(self, individual.id, {
        get: function () { return individual; },
        configurable: true
      });
      return self;
    };

    self.remove = function (id) {
      keys.splice(keys.indexOf(id), 1);
      delete self[id];
      return self;
    };

    Object.defineProperty(self, "length", {
      get: function () { return keys.length; },
      configurable: false
    });

    return self;
  };

});
