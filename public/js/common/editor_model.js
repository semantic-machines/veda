// Editor Model

"use strict";

function EditorModel(veda, document, container) {

	var self = document;
	
	veda.trigger("editor:loaded", self, container);
	return self;
};
