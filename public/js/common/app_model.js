// Veda application Model

;(function (veda) { "use strict";

  veda.AppModel = function (config) {

    var self = riot.observable(this);

    self.config = config;
    self.ticket = self.ticket || "";
    self.ontology = {};
    self.cache = {
      limit: 20000,
      count: 0,
      delta: 1000,
      storage: {},
      expire: {},
      get: function (key) {
        return this.storage[key];
      },
      add: function (obj, expires) {
        var that = this;
        var count = this.count;
        var limit = this.limit;
        var delta = this.delta;
        if ( count >= limit ) {
          var keys = Object.keys(this.expire).sort();
          for (var i = 0; limit - count < delta; i++) {
            var key = keys[ i ];
            this.expire[ key ] = this.expire[ key ].filter(function (obj) {
              if (limit - count >= delta) { return true; }
              delete that.storage[ obj.id ];
              count--;
              return false;
            });
            if (this.expire[key].length === 0) {
              delete this.expire[key];
            }
          }
          this.count = count;
          console.log("veda.cache limit (" + this.limit + " elements) reached, " + this.delta + " removed.");
        }
        var expire_key = typeof expires === "number" ? expires : Date.now();
        obj.expires = expire_key;
        this.storage[ obj.id ] = obj;
        this.expire[ expire_key ] = this.expire[ expire_key ] || [];
        this.expire[ expire_key ].push(obj);
        this.count++;
      },
      remove: function (key) {
        var that = this;
        var obj = this.storage[key];
        var expires = obj.expires;
        this.expire[expires] = this.expire[expires].filter(function (item) { return item.id !== key });
        if (this.expire[expires].length === 0) {
          delete this.expire[expires];
        }
        this.count--;
        return delete this.storage[key];
      },
      clear: function () {
        this.count = 0;
        this.storage = {};
        this.expire = {};
      }
    };

    // Define Model functions
    self.login = function (username, password) {
      var res = authenticate(username, password);
      self.ticket = res.id;
      if (!self.ticket) return;
      self.user_uri = res.user_uri;
      self.end_time = Math.floor((res.end_time - 621355968000000000) / 10000 );
      return {
        ticket: self.ticket,
        user_uri: self.user_uri,
        end_time: self.end_time
      };
    };

    self.logout = function() {
      self.user_uri = self.ticket = self.end_time = "";
      self.cache.clear();
      self.status = "logout";
      self.trigger("logout");
    };

    // Load ontology
    self.init = function () {
      try {
        self.ontology = new veda.OntologyModel();
        self.drafts = new veda.DraftsModel();
        self.user = new veda.UserModel("cfg:Guest");
      } catch (err) {
        delete self.ontology;
      }
    };

    // Start application
    self.start = function () {
      if ( !self.ontology ) {
        self.ontology = new veda.OntologyModel();
        self.drafts = new veda.DraftsModel();
      }
      self.user = new veda.UserModel(self.user_uri);
      self.status = "started";
      self.trigger("started");
    };

    return self;
  };

})(veda);
