// Ontology Model

"use strict";

function OntologyModel(veda) {

	function merge () {
		var c = {};
		for (var i in arguments) {
			for (var j in arguments[i]) c[j] = arguments[i][j];
		}
		return c;
	}

	var self = riot.observable(this);
	
	//self.cache = typeof localStorage != "undefined" ? localStorage : {};

	self.classes = {};
	self.properties = {};
	self.templates = {};
	self.specs = {};
	self.other = {};
	
	var individuals = {};
	for (var uri in veda.cache) {
		individuals[uri] = new IndividualModel(veda, uri);
	}
	for (var i in individuals) {
		switch ( individuals[i]["rdf:type"][0].id ) {
			case "rdfs:Class" :
			case "owl:Class" :
				self.classes[individuals[i].id] = new ClassModel(veda, individuals[i]);
				break
			case "rdf:Property" :
			case "owl:DatatypeProperty" :
			case "owl:ObjectProperty" :
				self.properties[individuals[i].id] = new PropertyModel(veda, individuals[i]);
				break
			case "v-ui:ClassTemplate" :
				self.templates[individuals[i].id] = individuals[i];
				break
			case "v-ui:PropertySpecification" :
				self.specs[individuals[i].id] = individuals[i];
				break
			default :
				self.other[individuals[i].id] = individuals[i];
				break
		}
	}


	self.dictionary = merge(self.classes, self.properties, self.templates, self.specs, self.other);
	
	return self;

};
