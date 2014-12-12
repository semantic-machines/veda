// Ontology Model

;(function (veda) { "use strict";

	veda.OntologyModel = function () {

		function merge () {
			var c = {};
			for (var i in arguments) {
				for (var j in arguments[i]) c[j] = arguments[i][j];
			}
			return c;
		}

		var self = riot.observable(this);
		
		self.classes = {};
		self.properties = {};
		self.templates = {};
		self.specs = {};
		self.other = {};

		var q = "'rdf:type' == 'rdfs:Class' || 'rdf:type' == 'owl:Class' || 'rdf:type' == 'rdfs:Datatype' || 'rdf:type' == 'owl:Ontology' ||" + // Classes
				"'rdf:type' == 'rdf:Property' || 'rdf:type' == 'owl:DatatypeProperty' || 'rdf:type' == 'owl:ObjectProperty' || " + // Properties
				"'rdf:type' == 'v-ui:ClassTemplate' || " + // Templates
				"'rdf:type' == 'v-ui:PropertySpecification'"; // Property specifications
		
		var q_results = query(veda.ticket, q);
		
		var unstored_uris = q_results.reduce( function (acc, item) {
			if ( !veda.storage[item] ) acc.push(item);
			else veda.dictionary[item] = new veda.IndividualModel(item);
			return acc;
		}, []);
		
		var unstored = get_individuals(veda.ticket, unstored_uris);
		unstored.map( function (item) {
			veda.storage[item["@"]] = JSON.stringify(item);
			veda.dictionary[item["@"]] = new veda.IndividualModel(item["@"]);
		});

		Object.keys(veda.dictionary).map( function (uri) {
			var individual = veda.dictionary[uri];
			switch ( individual["rdf:type"][0].id ) {
				case "rdfs:Class" :
				case "owl:Class" :
					self.classes[individual.id] = individual;
					break
				case "rdf:Property" :
				case "owl:DatatypeProperty" :
				case "owl:ObjectProperty" :
					self.properties[individual.id] = individual;
					break
				case "v-ui:ClassTemplate" :
					self.templates[individual.id] = individual;
					break
				case "v-ui:PropertySpecification" :
					self.specs[individual.id] = individual;
					break
				default :
					self.other[individual.id] = individual;
					break
			}
		});

		Object.keys(self.classes).map( function (uri) {
			var _class = self.classes[uri];
			if (!_class["rdfs:subClassOf"]) return;
			_class["rdfs:subClassOf"].map( function ( item ) {
				item.subClasses = item.subClasses || {};
				item.subClasses[_class.id] = _class;
			});
		});

		Object.keys(self.properties).map( function (uri) {
			var property = self.properties[uri];
			if (!property["rdfs:domain"]) return;
			property["rdfs:domain"].map( function ( item ) {
				(function fillDomainProperty (_class) {
					_class.domainProperties = _class.domainProperties || {};
					_class.domainProperties[property.id] = property;
					if (_class.subClasses && Object.keys(_class.subClasses).length) {
						Object.keys(_class.subClasses).map( function (subClass_uri) {
							fillDomainProperty (_class.subClasses[subClass_uri]);
						});
					}
				})(item);
			});
		});

		Object.keys(self.templates).map( function (uri) {
			var template = self.templates[uri];
			if (!template["v-ui:forClass"]) return; 
			template["v-ui:forClass"].map( function ( item ) {
				item.documentTemplate = item.documentTemplate || {};
				item.documentTemplate = template;
			});
		});

		Object.keys(self.specs).map( function (uri) {
			var spec = self.specs[uri];
			if (!spec["v-ui:forClass"]) return;
			spec["v-ui:forClass"].map( function ( item ) {
				item.specsByProps = item.specsByProps || {};
				item.specsByProps[spec["v-ui:forProperty"][0].id] = spec;
			});
		});

		return self;
			
	};

}(veda));
