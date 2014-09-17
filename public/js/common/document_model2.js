// Document Model

"use strict";

function DocumentModel2(veda, individual, container) {

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
			classTree.classes[_class.id] = _class;
			if (_class["rdfs:subClassOf"]) {
				buildClassTree(
					_class["rdfs:subClassOf"].map(function (item) {
						if (!classTree.classes[item.id]) {
							var res;
							res = new ClassModel(veda, item);
							res.subClasses = [_class];
							classTree.classes[item.id] = res;
							return res;
						} else {
							classTree.classes[item.id].subClasses.push(_class);
							return classTree.classes[item.id];
						}
					}), classTree);
			} else {
				classTree.roots[_class.id] = _class.id;
			}
		});
		return classTree;
	})( self.directClasses, self.classTree );

	// Add base rdfs:Resource class if not present
	if (!self.classTree.classes["rdfs:Resource"]) {
		self.classTree.classes["rdfs:Resource"] = new ClassModel(veda, "rdfs:Resource");
		self.classTree.roots["rdfs:Resource"] = "rdfs:Resource";
	}

	veda.trigger("document2:loaded", self, container);
	return self;
};
