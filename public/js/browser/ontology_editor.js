// Individual Presenter

veda.Module(function IndividualPresenter(veda) { "use strict";

	var template = $("#ontology-editor-template").html();

	var chosen_ontology = "";

	veda.on("load:ontology", function () {
		var container = $("#main");
		var tmpl = $(template);
		container.empty().append(tmpl);

		var $sidebar = $(".sidebar", tmpl);
		var $mainbar = $(".mainbar", tmpl);
		var $ontology_container = $("#ontology-container", tmpl);
		var $ontologies_list = $("#ontologies-list", tmpl);
		var expand = $("#expand", tmpl);

		expand.click(function () {
			$sidebar.toggleClass("hidden");
			$mainbar.toggleClass("col-md-9 col-md-12 col-md-offset-3");
			expand.toggleClass("glyphicon-resize-full glyphicon-resize-small");
		});

		$ontologies_list.on("click", "[href]", function (e) {
			e.preventDefault();
			var $this = $(this);
			var uri = $this.children().attr("resource");
			var ontology = new veda.IndividualModel(uri);
			$ontology_container.empty();
			ontology.present($ontology_container);
			chosen_ontology = ontology.id;
			$(".active", $ontologies_list).removeClass("active");
			$this.parent().addClass("active");
		});

		for (var uri in veda.ontology.ontologies) {
			var ontology = veda.ontology.ontologies[uri];
			var $li = $("<li role='presentation'>").appendTo($ontologies_list);
			var $a = $("<a href='#'>").appendTo($li);
			var tmp = new veda.IndividualModel("v-ui:LabelTemplate");
			ontology.present($a, tmp);
			if (uri === chosen_ontology) { $a.click(); }
		}
		if (!chosen_ontology) { $ontologies_list.children().first().children().first().click(); }

	});

});
