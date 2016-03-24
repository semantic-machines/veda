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

		if (veda.drafts.length) {
			Object.keys(veda.drafts).map(function (uri) {
				var li = $("<li>").appendTo(ol);
				var individual = veda.drafts[uri];
				var tmpl = new veda.IndividualModel("v-ui:LabelBlockLinkTemplate");
				individual.present(li, tmpl);
			});
		}
	});

	veda.on("update:drafts", function (drafts) {
		$("#drafts-counter").text(drafts.length);
	});

});
