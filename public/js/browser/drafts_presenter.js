// Drafts Presenter

veda.Module(function DraftsPresenter(veda) { "use strict";

  var template = $("#drafts-template").html();

  veda.on("load:drafts", function () {
    var container = $("#main");
    var tmpl = $(template);
    var ol = $("#drafts-list", tmpl);
    var deleteAllBtn = $("#delete-all", tmpl).click( function () {
      ol.empty();
      veda.drafts.clear();
    });
    container.empty().append(tmpl);

    var title = new veda.IndividualModel("v-s:Drafts");
    var deleteAll = new veda.IndividualModel("v-s:DeleteAll");
    title.present( $("#drafts-title", tmpl), new veda.IndividualModel("v-ui:LabelTemplate") );
    deleteAll.present( $("#delete-all", tmpl), new veda.IndividualModel("v-ui:LabelTemplate") );

    var tree = {};
    var linkTmpl = new veda.IndividualModel("v-ui:ClassNameLabelLinkTemplate");
    var labelTmpl = new veda.IndividualModel("v-ui:LabelTemplate");

    if (veda.drafts.length) {
      Object.keys(veda.drafts).map(function (uri) {
        var draft = veda.drafts[uri],
          parent = draft.parent;
        if ( parent ) {
          tree[parent] ? tree[parent].push(uri) : tree[parent] = [uri];
        } else {
          tree["root"] ? tree["root"].push(uri) : tree["root"] = [uri];
        }
      });
      renderDraftsTree(tree.root, ol, linkTmpl);
    }

    function renderDraftsTree(list, el, tmpl) {
      if (!list || !list.length) return;
      list.map(function (uri) {
        var draft = veda.drafts.get(uri);
        if (draft) {
          var li = $("<li>").appendTo(el);
          draft.present(li, tmpl);
          var ul = $("<ul>").appendTo(el);
          renderDraftsTree(tree[uri], ul, labelTmpl);
        }
      });
    }
  });

  veda.on("update:drafts", function (drafts) {
    $("#drafts-counter").text(drafts.length);
  });

});
