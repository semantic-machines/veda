veda.Module(function SearchSelectedFunctions(veda) { "use strict";
	var tmpl = $("#selected-actions-template").html();
	veda.on("search:complete", function (search, container_param, page) {
		var container = container_param || $("#main"),
			holder = $("#selected-actions-" + search.id, container),
			actions = $( tmpl ),
			del = $("#delete", actions),
			ttl = $("#export-ttl", actions);
		del.click(function () {
			if ( confirm("Вы действительно хотите удалить выбранные элементы?") ) {
				var l = new veda.IndividualListModel(search.selected);
				l.each(function (item) {
					item.delete();
				});
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
