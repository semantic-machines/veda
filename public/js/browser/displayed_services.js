/**

Displayed individuals services

 */

veda.Module(function DisplayedServices(veda) { "use strict";

  // Autoupdate displayed individuals that were changed on server

  veda.on("started", function () {
    var updateService = new veda.UpdateService();
    updateService.start();
  }).on("logout", function () {
    var updateService = new veda.UpdateService();
    updateService.stop();
  });

  veda.on("individual:loaded", updateWatch);

  function updateWatch(individual) {
    individual.one("individual:templateReady", subscribeDisplayed);
  }

  function subscribeDisplayed(template) {
    var individual = this;
    var updateService = new veda.UpdateService();
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
    var individual = this;

    veda.on("language:changed", localizeIndividual);
    template.one("remove", function () {
      veda.off("language:changed", localizeIndividual);
    });

    function localizeIndividual () {
      for (var property_uri in individual.properties) {
        if (property_uri === "@") { continue; }
        if ( individual.hasValue(property_uri) && individual.properties[property_uri][0].type === "String" ) {
          individual.trigger("propertyModified", property_uri, individual.get(property_uri));
          individual.trigger(property_uri, individual.get(property_uri));
        }
      }
    }
  }
});
