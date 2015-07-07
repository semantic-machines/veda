// Ontology Model

veda.Module(function (veda) { "use strict";

	/* owl:Thing && rdfs:Resource domain properties */
	var stopList = [
		//"rdf:type",
		//"rdfs:comment",
		//"rdfs:label",
		//"v-s:deleted",
		"owl:annotatedProperty",
		"owl:annotatedSource",
		"owl:annotatedTarget",
		"owl:bottomDataProperty",
		"owl:bottomObjectProperty",
		"owl:deprecated",
		"owl:differentFrom",
		"owl:members",
		"owl:sameAs",
		"owl:topObjectProperty",
		"owl:topDataProperty",
		"owl:versionInfo",
		"rdf:value",
		"rdfs:isDefinedBy",
		"rdfs:member",
		"rdfs:seeAlso"
	];

	veda.OntologyModel = function () {

		var self = this;
		
		self.classes = {};
		self.properties = {};
		self.specs = {};
		self.models = {};
		self.templates = {};
		self.other = {};
		
		var storage = typeof localStorage != 'undefined' ? localStorage : undefined;
		
		var q = /* Classes */ 
				"'rdf:type' == 'rdfs:Class' || " +
				"'rdf:type' == 'owl:Class' || " +
				"'rdf:type' == 'rdfs:Datatype' || " +
				"'rdf:type' == 'owl:Ontology' || " +
				/* Properties */
				"'rdf:type' == 'rdf:Property' || " +
				"'rdf:type' == 'owl:DatatypeProperty' || " +
				"'rdf:type' == 'owl:ObjectProperty' || " +
				"'rdf:type' == 'owl:OntologyProperty' || " +
				"'rdf:type' == 'owl:AnnotationProperty' || " +
				/* Models */
				"'rdf:type' == 'v-s:ClassModel' || " +
				/* Templates */
				"'rdf:type' == 'v-ui:ClassTemplate' || " +
				/* Property specifications */
				"'rdf:type' == 'v-ui:PropertySpecification' || " +
				"'rdf:type' == 'v-ui:IntegerPropertySpecification' || " + 
				"'rdf:type' == 'v-ui:DecimalPropertySpecification' || " +
				"'rdf:type' == 'v-ui:DatetimePropertySpecification' || " +
				"'rdf:type' == 'v-ui:StringPropertySpecification' || " +
				"'rdf:type' == 'v-ui:BooleanPropertySpecification' || " +
				"'rdf:type' == 'v-ui:ObjectPropertySpecification'";
		
		var q_results = query(veda.ticket, q);
		
		if (storage) {
			var unstored_uris = q_results.reduce( function (acc, item) {
				if ( !storage[item] ) { 
					acc.push(item);
				} else { 
					var individual = new veda.IndividualModel( JSON.parse(storage[item]), undefined, undefined, undefined, true, false );
					self[item] = individual;
				}
				return acc;
			}, []);
			
			var unstored = unstored_uris.length ? get_individuals(veda.ticket, unstored_uris) : [];
			unstored.map( function (item) {
				storage[ item["@"] ] = JSON.stringify(item);
				var individual = new veda.IndividualModel( item, undefined, undefined, undefined, true, false );
				self[ item["@"] ] = individual;
			});

		} else {
			get_individuals(veda.ticket, q_results).map( function (item) {
				self[ item["@"] ] = new veda.IndividualModel( item, undefined, undefined, undefined, true, false );
			});
		}
		
		q_results.map( function (uri) {
			var individual = self[uri];
			
			// Update localStorage after individual was saved
			individual.on("individual:afterSave", function (data) {
				storage[uri] = data;
			});
			
			switch ( individual["rdf:type"][0].id ) {
				case "rdfs:Class" :
				case "owl:Class" :
					self.classes[individual.id] = individual;
					break;
				case "rdf:Property" :
				case "owl:DatatypeProperty" :
				case "owl:ObjectProperty" :
				case "owl:OntologyProperty" :
				case "owl:AnnotationProperty" :
					self.properties[individual.id] = individual;
					break;
				case "v-ui:PropertySpecification" :
				case "v-ui:IntegerPropertySpecification" :
				case "v-ui:DecimalPropertySpecification" :
				case "v-ui:DatetimePropertySpecification" :
				case "v-ui:StringPropertySpecification" :
				case "v-ui:BooleanPropertySpecification" :
				case "v-ui:ObjectPropertySpecification" :
					self.specs[individual.id] = individual;
					break;
				case "v-s:ClassModel" :
					self.models[individual.id] = individual;
					break;
				case "v-ui:ClassTemplate" :
					self.templates[individual.id] = individual;
					break;
				default :
					self.other[individual.id] = individual;
					break;
			}
		});

		Object.keys(self.classes).map( function (uri) {
			var _class = self.classes[uri];
			// rdfs:Resource is a top level class
			if ( _class.id === "rdfs:Resource" ) return;
			// If class is not a subclass of another then make it a subclass of rdfs:Resource
			if ( !_class.hasValue("rdfs:subClassOf") ) {
				_class.defineProperty("rdfs:subClassOf");
				_class["rdfs:subClassOf"] = [ self["rdfs:Resource"] ];
			}
			_class["rdfs:subClassOf"].map( function ( item ) {
				item.subClasses = item.subClasses || {};
				item.subClasses[_class.id] = _class;
			});
		});

		Object.keys(self.properties).map( function (uri) {
			if (stopList.indexOf(uri) >= 0) return;
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

		Object.keys(self.specs).map( function (uri) {
			var spec = self.specs[uri];
			if (!spec["v-ui:forClass"]) return;
			spec["v-ui:forClass"].map( function ( _class ) {
				_class.specsByProps = _class.specsByProps || {};
				spec["v-ui:forProperty"].map( function (prop) {
					_class.specsByProps[prop.id] = spec;
				});
			});
		});

		Object.keys(self.templates).map( function (uri) {
			var template = self.templates[uri];
			if (!template["v-ui:forClass"]) return; 
			template["v-ui:forClass"].map( function ( item ) {
				item.template = template;
			});
		});

		Object.keys(self.models).map( function (uri) {
			var model = self.models[uri];
			if (!model["v-ui:forClass"]) return; 
			model["v-ui:forClass"].map( function ( item ) {
				item.model = model;
			});
		});

		// Initialize ontology objects
		q_results.map( function (uri) {
			self[uri].init();
		});

		return self;
			
	};

});
