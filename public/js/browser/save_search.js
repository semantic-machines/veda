// Save Search Presenter

veda.Module(function SaveSearch(veda) { "use strict";

  var template = $("#save-search-template").html();

  veda.on("search:rendered", function (search, container_param) {

    var btn = $( template );
    var container = container_param || $("#main");
    var qActions = $("#q-actions", container);

    qActions.prepend(btn);

    btn.on("click", function () {
      if (search.q) {

        var self = $(this);
        var ssContainer = $("<div/>");
        var ss = new veda.IndividualModel();
        ss["rdf:type"] = [new veda.IndividualModel("v-s:SavedSearch")];
        ss["v-s:author"] = [veda.user];
        ss["v-s:created"] = [new Date()];
        ss["v-s:query"] = [search.q];
        ss["rdfs:label"] = [search.q];
        ss.present(ssContainer, undefined, "edit");

        ss.on("afterSave afterReset", function () {
          self.popover("destroy");
        });

        self.popover({
          html: true,
          content: ssContainer,
          placement: "auto",
          container: qActions
        }).popover("show").on('hidden.bs.popover', function () {
          self.popover("destroy");
        });

      }
    });
  });
});
