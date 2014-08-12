// Document Model

"use strict";

function DocumentModel(veda, individual, container) {

	var self = individual instanceof IndividualModel ? individual : new IndividualModel(veda, individual);

	self.classTree = {roots:{}, classes:{}};

	self.directClasses = self["rdf:type"]
		.filter(function (item) {
			return item instanceof IndividualModel;
		})
		.map( function (item) {
			return new ClassModel(veda, item); 
		});

	self.classTree = (function buildClassTree (classes, classTree) {
		classes.map( function (_class) {
			classTree.classes[_class["@"]] = _class;
			if (_class["rdfs:subClassOf"]) {
				buildClassTree(
					_class["rdfs:subClassOf"].map(function (item) {
						if (!classTree.classes[item["@"]]) {
							var res;
							res = new ClassModel(veda, item);
							res.subClasses = [_class];
							classTree.classes[item["@"]] = res;
							return res;
						} else {
							classTree.classes[item["@"]].subClasses.push(_class);
							return classTree.classes[item["@"]];
						}
					}), classTree);
			} else {
				classTree.roots[_class["@"]] = _class["@"];
			}
		});
		return classTree;
	})( self.directClasses, self.classTree );

	// Add base rdfs:Resource class if not present
	if (!self.classTree.classes["rdfs:Resource"]) {
		self.classTree.classes["rdfs:Resource"] = new ClassModel(veda, "rdfs:Resource");
		self.classTree.roots["rdfs:Resource"] = "rdfs:Resource";
	}

	veda.trigger("document:loaded", self, container);
	return self;
};
