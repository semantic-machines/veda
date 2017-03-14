veda.Module(function SearchSelectedFunctions(veda) { "use strict";
  var tmpl = $("#selected-actions-template").html();
  veda.on("search:complete", function (search, container_param, page) {
    var container = container_param || $("#main"),
      holder = $("#selected-actions-" + search.id, container),
      actions = $( tmpl ),
      upd = $("#update", actions),
      del = $("#delete", actions),
      ttl = $("#export-ttl", actions);
    upd.click(function () {
      var i = 0, keys = Object.keys(search.selected), length = keys.length;
      function updateList(start) {
        try {
          for (i=start; i<length; i++) {
            var key = keys[i],
              item = search.selected[key];
            item.isSync(false);
            item.save();
          }
        } catch (ex) {
          setTimeout(function () {
            updateList(i);
          }, 2000);
        }
      }
      if ( confirm("Вы действительно хотите обновить выбранные элементы?") ) {
        updateList(0);
      }
    });
    del.click(function () {
      var i = 0, keys = Object.keys(search.selected), length = keys.length;
      function deleteList(start) {
        try {
          for (i=start; i<length; i++) {
            var key = keys[i],
              item = search.selected[key];
            item.delete();
          }
        } catch (ex) {
          setTimeout(function () {
            deleteList(i);
          }, 2000);
        }
      }
      if ( confirm("Вы действительно хотите удалить выбранные элементы?") ) {
        deleteList(0);
      }
    });
    ttl.click(function () {
      var l = new veda.IndividualListModel(search.selected);
      veda.Util.exportTTL(l);
    });
    search.on("search:selected", function () {
      if ( Object.getOwnPropertyNames(search.selected).length ) {
        if ( !$("#selected-actions", holder).length ) {
          holder.append(actions);
        }
      } else {
        actions.detach();
      }
    });
  });
});
