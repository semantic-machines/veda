// Document Model

"use strict";

function DocumentModel(veda, individual, container) {

	var self = individual instanceof IndividualModel ? individual : new IndividualModel(veda, individual);

	self.classTree = {roots:{}, classes:{}};
	self.classTree = (function buildClassTree (_classes, classTree) {
		_classes.map( function (_class) {
			classTree.classes[_class["@"]] = _class;
			if (_class["rdfs:subClassOf"]) {
				buildClassTree(
					_class["rdfs:subClassOf"].map(function (item) {
						if (!classTree.classes[item["@"]]) {
							var res;
							res = new ClassModel(veda, item);
							res.subclasses = [_class];
							classTree.classes[item["@"]] = res;
							return res;
						} else {
							classTree.classes[item["@"]].subclasses.push(_class);
							return classTree.classes[item["@"]];
						}
					}), classTree);
			} else {
				classTree.roots[_class["@"]] = _class["@"];
			}
		});
		return classTree;
	})(self["rdf:type"].map( function (item) { return new ClassModel(veda, item); } ), self.classTree);
	
	// Add base rdfs:Resource class if not present
	if (!self.classTree.classes["rdfs:Resource"]) {
		self.classTree.classes["rdfs:Resource"] = new ClassModel(veda, "rdfs:Resource");
		self.classTree.roots["rdfs:Resource"] = "rdfs:Resource";
	}
	
	veda.trigger("document:loaded", self, container);
	return self;
};
