// User Model

veda.Module(function (veda) { "use strict";

  veda.UserModel = function (uri) {

    var self = new veda.IndividualModel(uri);

    try {
      if ( self["rdf:type"][0].id !== "v-s:Person" ) { return self; }
    } catch (err) {
      localStorage.clear();
      location.reload();
    }
    veda.user = self;

    var langs = (new veda.IndividualModel("v-ui:AvailableLanguage"))["rdf:value"];
    self.availableLanguages = langs.reduce (
      function (acc, language) {
        var name = language["rdf:value"][0];
        acc[name] = language;
        return acc;
      }, {});
    self.defaultLanguage = (new veda.IndividualModel("v-ui:DefaultLanguage"))["rdf:value"][0]["rdf:value"][0].toString();

    if (uri === "cfg:Guest") {
      self.language = {};
      self.language[self.defaultLanguage] = self.availableLanguages[self.defaultLanguage];
      return self;
    }

    if (self.hasValue("v-s:defaultAppointment")) {
      veda.appointment = self["v-s:defaultAppointment"][0];
    } else if (self.hasValue("v-s:hasAppointment")) {
      self["v-s:defaultAppointment"] = [ self["v-s:hasAppointment"][0] ];
      veda.appointment = self["v-s:defaultAppointment"][0];
      self.save();
    } else {
      veda.appointment = undefined;
    }

    self.on("v-s:defaultAppointment", function (values) {
      if (self.hasValue("v-s:defaultAppointment")) {
        veda.appointment = self["v-s:defaultAppointment"][0];
      }
    });

    if ( self.hasValue("v-ui:hasPreferences") ) {
      self.preferences = self["v-ui:hasPreferences"][0];
      if ( !self.preferences.hasValue("v-ui:preferredLanguage") || !self.preferences.hasValue("v-ui:displayedElements")) {
        self.preferences["v-ui:preferredLanguage"] = [ self.availableLanguages[self.defaultLanguage] ];
        self.preferences["v-ui:displayedElements"] = [ 10 ];
        self.preferences.save();
      }
    } else {
      var preferences_id = self.id + "_pref";
      self.preferences = new veda.IndividualModel(preferences_id);
      if ( self.preferences.isNew() ) {
        self.preferences["v-s:author"] = [ self ];
        self.preferences["rdf:type"] = [ new veda.IndividualModel("v-ui:Preferences") ];
        self.preferences["rdfs:label"] = [ "Preferences_" + self.id ];
        self.preferences["v-ui:preferredLanguage"] = [ self.availableLanguages["RU"] ];
        self.preferences["v-ui:displayedElements"] = [ 10 ];
        self.preferences.save();
      }
      self["v-ui:hasPreferences"] = [ self.preferences ];
      self.save();
    }
    self.language = self.preferences["v-ui:preferredLanguage"].reduce( function (acc, lang) {
      acc[lang["rdf:value"][0]] = self.availableLanguages[lang["rdf:value"][0]];
      return acc;
    }, {} );
    self.displayedElements = self.preferences["v-ui:displayedElements"][0];

    if ( self.hasValue("v-s:hasAspect") ) {
      self.aspect = self["v-s:hasAspect"][0];
    } else {
      var aspect_id = self.id + "_aspect";
      self.aspect = new veda.IndividualModel(aspect_id);
      if ( self.aspect.isNew() ) {
        self.aspect["rdf:type"] = [ new veda.IndividualModel("v-s:PersonalAspect") ];
        self.aspect["v-s:owner"] = [ self ];
        self.aspect["rdfs:label"] = [ "PersonalAspect_" + self.id ];
        self.aspect.save();
      }
      self["v-s:hasAspect"] = [ self.aspect ];
      self.save();
    }

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

    return self;
  };

});
