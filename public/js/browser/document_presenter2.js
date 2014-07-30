// Document Presenter
Veda(function DocumentPresenter(veda) { "use strict";

	// Get templates
	var doc_tmpl = Hogan.compile( $("#doc-tmpl").html() );
	
	veda.on("document:loaded", function (document, container_param) {
		
		var container = container_param || $("#main2");
		localize(container, veda.user.language);
		
		container.html( doc_tmpl.render(document) );
		
	});

});
