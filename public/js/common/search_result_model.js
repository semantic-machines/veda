// Search result Model

veda.Module(function (veda) { "use strict";

	veda.SearchResultModel = function (uri, container, template) {

		var individual = new veda.IndividualModel(uri);
		
		var self = riot.observable( Object.create(individual) );
		
		veda.trigger("search_result:loaded", self, container, template);

		return self;
		
	};

});
