// Save Search Presenter

veda.Module(function SavedSearchList(veda) { "use strict";

  var template = $("#saved-search-list-button-template").html();

  veda.on("search:rendered", function (search, container_param) {

    var q = "'rdf:type'=='v-s:SavedSearch'&&('v-s:author'=='" + veda.user.id + "'||'v-s:author'=='cfg:VedaSystemAccount')";

    var btn = $( template );
    var container = container_param || $("#main");
    var qActions = $("#q-actions", container);

    //$("#saved-search-list", qActions).remove();
    qActions.prepend(btn);

    var sContainer = $("<div/>", {text:"Данные отсутствуют"});
    btn.popover({
      html: true,
      content: sContainer,
      placement: "bottom",
      container: qActions
    });

    var l = $("<div>", {"class": "list-group no-margin"});
    var tmpl = new veda.IndividualModel("v-ui:LabelTemplate");
    btn.one("click", function () {
      var s = new veda.SearchModel(undefined, null);
      s.off("*"); // Prevent SearchPresenter events
      s.search(q);
      Object.getOwnPropertyNames(s.results).map( function (id) {
        var a = $("<a>", {"class": "list-group-item no-border", "href": "", "style": "display: block"}).appendTo(l);

        var d = s.results[id].present(a, tmpl);
        if (search.q == d["v-s:query"][0]) a.addClass("active");
        a.click(function (e) {
          e.preventDefault();
          $("a", sContainer).removeClass("active");
          search.q = d["v-s:query"][0];

          if (container.prop("id") === "main") riot.route("#/search/" + search.q);

          search.search();
          a.addClass("active");
          btn.popover("hide");
        });
        var b = $("<span>", {"class": "badge"}).prependTo(a);
        var i = $("<i>", {"class": "glyphicon glyphicon-remove"}).appendTo(b);
        b.click(function (e) {
          e.preventDefault();
          d["v-s:deleted"] = [ true ];
          d.save();
          a.remove();
          if ( !$("a", qActions).length ) btn.popover("hide");
        });
      });
      if (s.results_count) sContainer.html(l);
    });
  });
});
