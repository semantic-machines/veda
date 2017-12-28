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
      self.cache = {};
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
