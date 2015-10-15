// Menu Presenter
veda.Module(function MenuPresenter(veda) { "use strict";
	var container = $("#user-menu");
	veda.on("started", function () {
		container.empty();
		var template = new veda.IndividualModel("v-m:MenuViewTemplate");
		var menu = new veda.IndividualModel("v-m:MainMenu", container, template);
	});
	veda.on("login:failed", function () {
		container.empty();
	});
});
