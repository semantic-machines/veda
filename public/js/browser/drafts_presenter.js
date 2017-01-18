// Drafts Presenter

veda.Module(function DraftsPresenter(veda) { "use strict";

  var template = $("#drafts-template").html();

  veda.on("load:drafts", function () {
    var container = $("#main");
    var tmpl = $(template);
    var ol = $("#drafts-list", tmpl);
    var deleteAllBtn = $("#delete-all", tmpl).click( function () {
      if ( veda.drafts.length && confirm("Вы уверены? / Are you sure?") ) {
        ol.empty();
        veda.drafts.clear();
      }
    });
    container.empty().append(tmpl);

    var title = new veda.IndividualModel("v-s:Drafts");
    var deleteAll = new veda.IndividualModel("v-s:DeleteAll");
    title.present( $("#drafts-title", tmpl), new veda.IndividualModel("v-ui:LabelTemplate") );
    deleteAll.present( $("#delete-all", tmpl), new veda.IndividualModel("v-ui:LabelTemplate") );

    var tree = {};
    var linkTmpl = new veda.IndividualModel("v-ui:ClassNameLabelLinkTemplate");
    var labelTmpl = new veda.IndividualModel("v-ui:ClassNameLabelTemplate");

    if (veda.drafts.length) {
      Object.keys(veda.drafts).map(function (uri) {
        var draft = veda.drafts[uri],
          parent = draft.parent;
        if ( parent && veda.drafts[parent] ) {
          tree[parent] ? tree[parent].push(uri) : tree[parent] = [uri];
        } else {
          tree["root"] ? tree["root"].push(uri) : tree["root"] = [uri];
        }
      });
      renderDraftsTree(tree.root, ol, linkTmpl);
    }

    tmpl.on("click", ".remove-draft", function (e) {
      e.stopPropagation();
      var uri = $(this).parent().find("[resource]").attr("resource");
      veda.drafts.remove(uri);
    });

    function renderDraftsTree(list, el, tmpl) {
      if (!list || !list.length) return;
      list.map(function (uri) {
        var draft = veda.drafts.get(uri);
        if (draft) {
          var li = $("<li>").appendTo(el);
          draft.present(li, tmpl);
          li.append("<button class='remove-draft btn btn-sm btn-link glyphicon glyphicon-remove' style='margin-top:-5px'></button>");
          var ul = $("<ul>").appendTo(el);
          renderDraftsTree(tree[uri], ul, labelTmpl);
        }
      });
    }
  });

  veda.on("update:drafts", function (drafts) {
    $("#drafts-counter").text(drafts.length);
    if (location.hash === "#/drafts") { veda.trigger("load:drafts") }
  });

  // Включим позже
  // Clear orphan drafts
  /*$(window).unload(function() {
    Object.keys(veda.drafts).map(function (uri) {
      var draft = veda.drafts[uri],
          parent = draft.parent;
      if ( parent && !veda.drafts[parent] ) {
        veda.drafts.remove(uri);
      }
    });
  });*/

});
