// User Model

veda.Module(function (veda) { "use strict";

  veda.UserModel = function (uri) {

    var self = veda.IndividualModel.call(this, uri);

    return self;

  }

  veda.UserModel.prototype = Object.create(veda.IndividualModel.prototype);

  veda.UserModel.prototype.constructor = veda.UserModel;

  var proto = veda.UserModel.prototype;

  proto._init = function () {
    var self = this;
    return this.initLanguage().then( function () {
      self.initPreferences();
    }).then( function () {
      self.initAppointment();
    }).then( function () {
      self.initAspect();
    }).then( function () {
      if ( !self.isSync() ) {
        self.save();
      }
    });
  };

  proto.initLanguage = function () {
    var self = this;

    return self.load().then( function () {
      var availableLanguageConfig = new veda.IndividualModel("v-ui:AvailableLanguage");
      return availableLanguageConfig.load();

    }).then(function (availableLanguageConfig) {
      var langsPromises = availableLanguageConfig["rdf:value"].map(function (lang) {
        return lang.load();
      });
      return Promise.all(langsPromises);

    }).then(function (langs) {
      self.availableLanguages = langs.reduce (
        function (acc, language) {
          var name = language["rdf:value"][0];
          acc[name] = language;
          return acc;
        }, {});

    }).then(function () {
      var defaultLanguageConfig = new veda.IndividualModel("v-ui:DefaultLanguage");
      return defaultLanguageConfig.load();

    }).then(function (defaultLanguageConfig) {
      var defaultLanguagePromise = defaultLanguageConfig["rdf:value"][0];
      return defaultLanguagePromise.load();

    }).then(function (defaultLanguage) {
      self.defaultLanguage = defaultLanguage["rdf:value"][0].toString();
    });
  };

  proto.initPreferences = function () {
    var self = this;
    return self.load().then( function () {
      if ( self.hasValue("v-ui:hasPreferences") ) {
        self.preferences = self["v-ui:hasPreferences"][0];
        return self.preferences.load();
      } else {
        var preferences_id = self.id + "_pref";
        self.preferences = new veda.IndividualModel(preferences_id);
        self.preferences["v-s:owner"] = [ self ];
        self.preferences["rdf:type"] = [ new veda.IndividualModel("v-ui:Preferences") ];
        self.preferences["rdfs:label"] = [ "Preferences_" + self.id ];
        self.preferences["v-ui:preferredLanguage"] = [ self.availableLanguages["RU"] ];
        self.preferences["v-ui:displayedElements"] = [ 10 ];
        self["v-ui:hasPreferences"] = [ self.preferences ];
        return self.preferences.save();
      }
    }).then(function () {

      if ( !self.preferences.hasValue("v-ui:preferredLanguage") || !self.preferences.hasValue("v-ui:displayedElements")) {
        self.preferences["v-ui:preferredLanguage"] = [ self.availableLanguages[self.defaultLanguage] ];
        self.preferences["v-ui:displayedElements"] = [ 10 ];
        self.preferences.save();
      }

      self.language = self.preferences["v-ui:preferredLanguage"].reduce( function (acc, lang) {
        acc[lang["rdf:value"][0]] = self.availableLanguages[lang["rdf:value"][0]];
        return acc;
      }, {} );
      self.displayedElements = self.preferences["v-ui:displayedElements"][0];

      self.preferences.on("v-ui:displayedElements", function (values) {
        self.displayedElements = values[0];
      });

      self.preferences.on("v-ui:preferredLanguage", function (values) {
        self.language = values.reduce( function (acc, lang) {
          acc[lang["rdf:value"][0]] = self.availableLanguages[lang["rdf:value"][0]];
          return acc;
        }, {} );
        self.preferences.save();
        veda.trigger("language:changed");
      });

    });

  };

  proto.initAppointment = function () {
    var self = this;
    return this.load().then( function (self) {
      if (self.hasValue("v-s:defaultAppointment")) {
        veda.appointment = self["v-s:defaultAppointment"][0];
      } else if (self.hasValue("v-s:hasAppointment")) {
        self["v-s:defaultAppointment"] = [ self["v-s:hasAppointment"][0] ];
        veda.appointment = self["v-s:defaultAppointment"][0];
      } else {
        veda.appointment = undefined;
      }
    });
  };

  proto.initAspect = function () {
    var self = this;
    return this.load().then( function (self) {
      if ( self.hasValue("v-s:hasAspect") ) {
        self.aspect = self["v-s:hasAspect"][0];
      } else {
        var aspect_id = self.id + "_aspect";
        self.aspect = new veda.IndividualModel(aspect_id);
        self.aspect["rdf:type"] = [ new veda.IndividualModel("v-s:PersonalAspect") ];
        self.aspect["v-s:owner"] = [ self ];
        self.aspect["rdfs:label"] = [ "PersonalAspect_" + self.id ];
        self.aspect.save();
        self["v-s:hasAspect"] = [ self.aspect ];
      }
      self["v-s:hasAspect"] = [ self.aspect ];
      self.save();
    });

  };

});
