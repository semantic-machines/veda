/**

Displayed individuals services

 */

veda.Module(function DisplayedServices(veda) { "use strict";

  // Autoupdate displayed individuals that were changed on server

  var updateService = new veda.UpdateService();

  veda.on("started", function () {
    updateService.start();
  }).on("logout", function () {
    updateService.stop();
  });

  veda.on("individual:loaded", updateWatch);

  function updateWatch(individual) {
    individual.one("individual:templateReady", subscribeDisplayed);
  }

  function subscribeDisplayed(template) {
    var individual = this;
    updateService.subscribe(individual.id);

    template.one("remove", function () {
      updateService.unsubscribe(individual.id);
    });
  }

  // Re-read strings in individuals on language switch

  veda.on("individual:loaded", languageWatch);

  function languageWatch(individual) {
    individual.one("individual:templateReady", localizeDisplayed);
  }

  function localizeDisplayed(template) {
    var self = this;

    veda.on("language:changed", localizeIndividual);
    template.one("remove", function () {
      veda.off("language:changed", localizeIndividual);
    });

    function localizeIndividual () {
      for (var property_uri in self.properties) {
        if (property_uri === "@" || property_uri === "rdf:type") { continue; }
        var property = new veda.IndividualModel(property_uri),
            range = property.hasValue("rdfs:range") ?  property["rdfs:range"][0].id : undefined;
        if (range === "xsd:string" || range === "rdfs:Literal" || range === "rdfs:Resource") {
          self.trigger("individual:propertyModified", property_uri, self[property_uri]);
        }
      }
    }
  }

});
