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
      self.ontology = {};
      self.drafts = {};
      self.status = "logout";
      self.trigger("logout");
    };

    self.load = function (page, params) {
      switch (page) {
        case "console":
          veda.Util.construct(veda.ConsoleModel, params);
          break;
        case "search":
          veda.Util.construct(veda.SearchModel, params);
          break;
        case "graph":
          self.trigger.apply(self, ["load:graph"].concat(params));
          break;
        case "drafts":
          self.trigger.apply(self, ["load:drafts"].concat(params));
          break;
        default:
          if (!params[0]) { params[0] = "#main"; }
          veda.Util.construct(veda.IndividualModel, [page].concat(params));
      }
    };

    // Load ontology
    self.init = function () {
      self.ontology = new veda.OntologyModel();
      self.user = new veda.UserModel(self.user_uri);
      self.drafts = new veda.DraftsModel();
      self.status = "started";
      self.trigger("started");
    };

    // Logout if server returned 470 or 471 error (ticket not found or expired)
    self.on("danger", function (msg) {
      if (msg.status === 470 || msg.status === 471) {
        veda.logout();
      }
    });

    return self;
  };

})(veda);
