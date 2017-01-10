// Search Model

veda.Module(function (veda) { "use strict";

  veda.SearchModel = function (q, container, queryPrefix, sort) {
    var self = riot.observable(this);
    var results_keys;

    self.id = guid();

    // Define Model data setters & getters
    var properties = {selectedType:{}, q:"", queryPrefix:"", sort:"", results:{}, results_count:undefined, selected:{}, query_time:undefined};
    for (var property in properties) {
      (function (property) {
        Object.defineProperty(self, property, {
          get: function () { return properties[property]; },
          set: function (value) {
            if (properties[property] == value) return;
            properties[property] = value;
            self.trigger("property:changed", property, properties[property]);
          }
        });
      })(property);
    }

    self.toggleSelected = function (i) {
      if (!self.results[ results_keys[i] ]) return self.selected;
      if (self.results[ results_keys[i] ].id in self.selected) {
        delete self.selected[self.results[ results_keys[i] ].id];
      } else {
        self.selected[self.results[ results_keys[i] ].id] = self.results[ results_keys[i] ];
      }
      self.trigger("search:selected");
      return self;
    };

    self.toggleAll = function () {
      if (Object.keys(self.selected).length != self.results_count) {
        for (var i=0; i < self.results_count; i++) {
          self.selected[self.results[ results_keys[i] ].id] = self.results[ results_keys[i] ];
        }
      } else {
        self.selected = {};
      }
      self.trigger("search:selected");
      return self;
    };

    // Define Model functions
    self.search = function (qq, databases, reopen) {
      reopen = !!reopen;
      self.q = qq || self.q;

      // Clear previous results
      self.results = {};
      var t1 = Date.now();
      q = self.q;

      // Transform user input like "roman karpov" to "'*'=='roman' && '*'=='karpov'"
      if (q && q.indexOf("==") < 0) {
        q = "(" + q.replace(/[-*]/g, " ").replace(/\s+/g, " ").trim().split(" ").map(function (t) { return "'*'=='" + t + "*'";}).join("&&") + ")";
      }

      // Prefix query if defined in constructor
      q = [self.queryPrefix, q].filter(function (item) { return !!item; }).join("&&") ;

      self.fullQuery = q;

      var results = query(veda.ticket, q, self.sort, databases, reopen).result;

      var t2 = Date.now();
      self.query_time = t2 - t1;
      for (var i in results) {
        (function(i){
          Object.defineProperty(self.results, results[i], {
            get: function () {
              if (typeof results[i] == 'object') return results[i];
              var cache = !reopen;
              return results[i] = new veda.IndividualModel(results[i], undefined, undefined, undefined, cache);
            },
            enumerable: true,
            configurable: true
          });
        })(i);
      }
      self.results_count = results.length;
      results_keys = Object.getOwnPropertyNames(self.results);
      self.trigger("search:complete");
      return self;
    };

    // Model messages
    self.on("search:complete", function () {
      veda.trigger("search:complete", self, container);
    });

    self.on("search:loaded", function () {
      veda.trigger("search:loaded", self, container);
    });

    self.trigger("search:loaded");

    // Search if params given
    self.q = q;
    self.queryPrefix = queryPrefix;
    self.sort = sort || "'rdfs:label_ru' asc , 'rdfs:label_en' asc , 'rdfs:label' asc";
    if (self.q || self.queryPrefix) self.search();

    return self;
  };

});
