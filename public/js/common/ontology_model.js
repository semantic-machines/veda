// Ontology Model

veda.Module(function (veda) { "use strict";

	var storage = typeof localStorage !== "undefined" ? localStorage : {
		clear: function () {
			var self = this;
			Object.keys(this).map(function (key) {
				if (typeof self[key] !== "function") delete self[key];
			});
		}
	}

	if (!storage.ontology) storage.clear();

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
		//"owl:sameAs",
		"owl:topObjectProperty",
		"owl:topDataProperty",
		"owl:versionInfo",
		//"rdf:value",
		"rdfs:isDefinedBy",
		"rdfs:member",
		"rdfs:seeAlso"
	];

	veda.OntologyModel = function () {

		// Initialization percentage
		veda.trigger("init:progress", 0);

		//var t1 = new Date();

		var self = this;

		var classes = {},
			properties = {},
			specs = {},
			models = {},
			templates = {},
			other = {};

		var ontology;
		try {
			ontology = JSON.parse(storage.ontology);
		} catch (e) {
			ontology = getOntology();
			storage.ontology = JSON.stringify(ontology);
		}

		// Check whether server & client cfg:OntoVsn objects are equal
		var clientVsn = ontology["cfg:OntoVsn"]["rdf:value"][0].data;
		var serverVsn = get_individual(veda.ticket, "cfg:OntoVsn")["rdf:value"][0].data;
		if ( clientVsn !== serverVsn ) {
			// Get ontology from server
			ontology = getOntology();
			storage.ontology = JSON.stringify(ontology);
		}

		// Initialize individual properties in {veda.IndividualModel.prototype}
		Object.keys(ontology).map(function (property_uri) {
			var individual = ontology[property_uri],
				type = individual["rdf:type"][0].data;
			if (
				(	type === 'rdf:Property'
					|| type === 'owl:DatatypeProperty'
					|| type === 'owl:ObjectProperty'
					|| type === 'owl:OntologyProperty'
					|| type === 'owl:AnnotationProperty'
				)
				&& !veda.IndividualModel.prototype.hasOwnProperty(property_uri)
			) {
				veda.IndividualModel.defineProperty(property_uri);
			}
		});

		// Construct ontology individuals
		Object.keys(ontology).map(function (key) {
			var individual = ontology[key];
			if (individual) self[key] = new veda.IndividualModel( individual, undefined, undefined, undefined, true, false );
		});

		// Initialization percentage
		veda.trigger("init:progress", 10);

		// Allocate ontology objects
		Object.keys(self).map( function (uri) {
			var individual = self[uri];
			if (!individual || !individual.id) return;

			switch ( individual["rdf:type"][0].id ) {
				case "rdfs:Class" :
				case "owl:Class" :
					classes[individual.id] = individual;
					break;
				case "rdf:Property" :
				case "owl:DatatypeProperty" :
				case "owl:ObjectProperty" :
				case "owl:OntologyProperty" :
				case "owl:AnnotationProperty" :
					properties[individual.id] = individual;
					break;
				case "v-ui:PropertySpecification" :
				case "v-ui:IntegerPropertySpecification" :
				case "v-ui:DecimalPropertySpecification" :
				case "v-ui:DatetimePropertySpecification" :
				case "v-ui:StringPropertySpecification" :
				case "v-ui:BooleanPropertySpecification" :
				case "v-ui:ObjectPropertySpecification" :
					specs[individual.id] = individual;
					break;
				case "v-s:ClassModel" :
					models[individual.id] = individual;
					break;
				case "v-ui:ClassTemplate" :
					templates[individual.id] = individual;
					break;
				default :
					other[individual.id] = individual;
					break;
			}
		});

		// Initialization percentage
		veda.trigger("init:progress", 20);

		// Process classes
		Object.keys(classes).map( function (uri) {
			var _class = classes[uri];
			// rdfs:Resource is a top level class
			if ( _class.id === "rdfs:Resource" ) return;
			// If class is not a subclass of another then make it a subclass of rdfs:Resource
			if ( !_class.hasValue("rdfs:subClassOf") ) {
				_class["rdfs:subClassOf"] = [ self["rdfs:Resource"] ];
			}
			_class["rdfs:subClassOf"].map( function ( item ) {
				item.subClasses = item.subClasses || {};
				item.subClasses[_class.id] = _class;
			});
		});

		// Initialization percentage
		veda.trigger("init:progress", 30);

		// Process properties
		Object.keys(properties).map( function (uri) {
			if (stopList.indexOf(uri) >= 0) return;
			var property = properties[uri];
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

		// Initialization percentage
		veda.trigger("init:progress", 60);

		// Process specifications
		Object.keys(specs).map( function (uri) {
			var spec = specs[uri];
			if (!spec["v-ui:forClass"]) return;
			spec["v-ui:forClass"].map( function ( _class ) {
				_class.specsByProps = _class.specsByProps || {};
				spec["v-ui:forProperty"].map( function (prop) {
					_class.specsByProps[prop.id] = spec;
				});
			});
		});

		// Initialization percentage
		veda.trigger("init:progress", 70);

		// Process templates
		Object.keys(templates).map( function (uri) {
			var template = templates[uri];
			if (!template["v-ui:forClass"]) return;
			template["v-ui:forClass"].map( function ( item ) {
				item.template = template;
			});
		});

		// Initialization percentage
		veda.trigger("init:progress", 80);

		// Process models
		Object.keys(models).map( function (uri) {
			var model = models[uri];
			if (!model["v-ui:forClass"]) return;
			model["v-ui:forClass"].map( function ( item ) {
				item.model = model;
			});
		});

		// Initialization percentage
		veda.trigger("init:progress", 90);

		// Initialization percentage
		Object.keys(self).map( function (uri) {
			var individual = self[uri];
			if (!individual || !individual.id) return;
			individual.init();
		});

		veda.trigger("init:progress", 100);

		//var t2 = new Date();
		//console.log("onto load", (t2-t1)/1000, "sec", storage.length);

		return self;

		// Get ontology from server
		function getOntology () {
			var q = /* Ontology version */
					"'@' == 'cfg:OntoVsn' || " +
					/* Classes */
					"'rdf:type' === 'rdfs:Class' || " +
					"'rdf:type' === 'owl:Class' || " +
					"'rdf:type' === 'rdfs:Datatype' || " +
					"'rdf:type' === 'owl:Ontology' || " +
					/* Properties */
					"'rdf:type' === 'rdf:Property' || " +
					"'rdf:type' === 'owl:DatatypeProperty' || " +
					"'rdf:type' === 'owl:ObjectProperty' || " +
					"'rdf:type' === 'owl:OntologyProperty' || " +
					"'rdf:type' === 'owl:AnnotationProperty' || " +
					/* Models */
					"'rdf:type' === 'v-s:ClassModel' || " +
					/* Templates */
					"'rdf:type' === 'v-ui:ClassTemplate' || " +
					/* Property specifications */
					"'rdf:type' === 'v-ui:PropertySpecification' || " +
					"'rdf:type' === 'v-ui:IntegerPropertySpecification' || " +
					"'rdf:type' === 'v-ui:DecimalPropertySpecification' || " +
					"'rdf:type' === 'v-ui:DatetimePropertySpecification' || " +
					"'rdf:type' === 'v-ui:StringPropertySpecification' || " +
					"'rdf:type' === 'v-ui:BooleanPropertySpecification' || " +
					"'rdf:type' === 'v-ui:ObjectPropertySpecification'";

			var result = {};
			get_individuals(veda.ticket, query(veda.ticket, q)).map( function (item) {
				result[ item["@"] ] = item;
			});
			return result;
		}
	};

});
