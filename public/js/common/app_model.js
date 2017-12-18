// Veda application Model

;(function (veda) { "use strict";

  veda.AppModel = function (config) {

    var self = riot.observable(this);

    self.config = config;
    self.user_uri = self.ticket = self.end_time = "";
    self.cache = {};
    self.ontology = {};

    // Define Model functions
    self.login = function (username, password) {
      return veda.Backend.authenticate(username, password).then(function (auth) {
        veda.ticket = auth.id;
        veda.user_uri = auth.user_uri;
        veda.end_time = Math.floor((auth.end_time - 621355968000000000) / 10000 );
        return {
          ticket: veda.ticket,
          user_uri: veda.user_uri,
          end_time: veda.end_time
        };
      });
    };

    self.logout = function() {
      self.user_uri = self.ticket = self.end_time = "";
      self.cache = {};
      self.trigger("logout");
    };

    // Load ontology
    self.init = function () {
      self.ontology = new veda.OntologyModel();
      return self.ontology.init().then(function () {
        self.user = new veda.UserModel("cfg:Guest");
        self.user._init();
      });
    };

    // Start application
    self.start = function () {
      self.user = new veda.UserModel(self.user_uri);
      return self.user._init().then(function () {
        self.trigger("started");
      });
    };

    return self;
  };

})(veda);
