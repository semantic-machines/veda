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

  veda.OntologyModel = function () {

    // Singleton pattern
    if (veda.OntologyModel.prototype._singletonInstance) {
      return veda.OntologyModel.prototype._singletonInstance;
    }
    veda.one("logout", function () {
      veda.OntologyModel.prototype._singletonInstance = null;
    });

    // Initialization percentage
    veda.trigger("init:progress", 0);

    var self = this;

    var ontology,
        classes = {},
        properties = {},
        specifications = {},
        classTree = {};

    try {
      ontology = JSON.parse(storage.ontology);
    } catch (e) {
      ontology = getOntology();
      storage.ontology = JSON.stringify(ontology);
    }

    // Check whether server & client cfg:OntoVsn objects are equal
    var clientVsn;
    try {
      clientVsn = ontology["cfg:OntoVsn"]["rdf:value"][0].data;
    } catch (ex) {
      clientVsn = undefined;
    }
    var serverVsn = get_individual(veda.ticket, "cfg:OntoVsn")["rdf:value"][0].data;
    if ( clientVsn !== serverVsn ) {
      // Get ontology from server
      ontology = getOntology();
      storage.ontology = JSON.stringify(ontology);
    }

    // Allocate ontology objects
    Object.keys(ontology).map( function (uri) {
      var individual_json = ontology[uri];
      var type = individual_json["rdf:type"][0].data;
      var individual = new veda.IndividualModel( individual_json, true, false );

      switch ( type ) {
        case "rdfs:Class" :
        case "owl:Class" :
          classes[uri] = individual;
          break;
        case "rdf:Property" :
        case "owl:DatatypeProperty" :
        case "owl:ObjectProperty" :
        case "owl:OntologyProperty" :
        case "owl:AnnotationProperty" :
          properties[uri] = individual;
          // Initialize individual properties in {veda.IndividualModel.prototype}
          if ( !veda.IndividualModel.prototype.hasOwnProperty(uri) ) {
            veda.IndividualModel.defineProperty(uri);
          }
          if ( !veda.IndividualModelAsync.prototype.hasOwnProperty(uri) ) {
            veda.IndividualModelAsync.defineProperty(uri);
          }
          break;
        case "v-ui:PropertySpecification" :
        case "v-ui:DatatypePropertySpecification" :
        case "v-ui:ObjectPropertySpecification" :
          specifications[uri] = individual;
          break;
      }
    });

    // Initialization percentage
    veda.trigger("init:progress", 20);

    // Process classes
    Object.keys(classes).map( function (uri) {
      var _class = classes[uri];
      // populate classTree
      if ( !classTree[_class.id] ) {
        classTree[_class.id] = {
          superClasses: [],
          properties: [],
          specifications: {}
        };
      }
      // rdfs:Resource is a top level class
      if ( _class.id === "rdfs:Resource" ) { return; }
      // If class is not a subclass of another then make it a subclass of rdfs:Resource
      if ( !_class.hasValue("rdfs:subClassOf") ) {
        _class["rdfs:subClassOf"] = [ classes["rdfs:Resource"] ];
        classTree[_class.id].superClasses.push("rdfs:Resource");
      }
      _class["rdfs:subClassOf"].map( function ( superClass ) {
        classTree[_class.id].superClasses.push( superClass.id );
      });
      /*if ( _class.hasValue("v-ui:hasModel") ) {
        _class.model = _class["v-ui:hasModel"][0];
      }*/
    });

    // Initialization percentage
    veda.trigger("init:progress", 40);

    // Process properties
    Object.keys(properties).map( function (uri) {
      var property = properties[uri];
      if (!property["rdfs:domain"]) { return; }
      property["rdfs:domain"].map( function ( _class ) {
        classTree[_class.id].properties.push(property.id);
      });
    });

    // Initialization percentage
    veda.trigger("init:progress", 60);

    // Process specifications
    Object.keys(specifications).map( function (uri) {
      var spec = specifications[uri];
      if (!spec["v-ui:forClass"]) { return; }
      spec["v-ui:forClass"].map( function ( _class ) {
        spec["v-ui:forProperty"].map( function (prop) {
          classTree[_class.id].specifications[prop.id] = spec.id;
        });
      });
    });

    // Initialization percentage
    veda.trigger("init:progress", 80);

    // Init class individuals
    Object.keys(classes).map( function (uri) {
      var _class = classes[uri];
      _class.init();
    });

    // Init property individuals
    Object.keys(properties).map( function (uri) {
      var property = properties[uri];
      property.init();
    });

    // Init specification individuals
    Object.keys(specifications).map( function (uri) {
      var spec = specifications[uri];
      spec.init();
    });

    veda.trigger("init:progress", 100);

    this.getClassProperties = function (_class_uri) {
      return veda.Util.unique( getProps(_class_uri) );
    }

    function getProps (_class_uri) {
      var _class = classTree[_class_uri];
      var props = _class.properties;
      return [].concat.apply( props, _class.superClasses.map( getProps ) );
    };

    this.getClassSpecifications = function getSpecs (_class_uri) {
      var _class = classTree[_class_uri];
      var specs = _class.specifications;
      var superSpecsArray = _class.superClasses.map( getSpecs );
      superSpecsArray.map( function (superSpecs) {
        for (var property_uri in superSpecs) {
          if ( !specs[property_uri] ) {
            specs[property_uri] = superSpecs[property_uri];
          }
        }
      });
      return specs;
    };

    return veda.OntologyModel.prototype._singletonInstance = self;

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
          /* Property specifications */
          "'rdf:type' === 'v-ui:PropertySpecification' || " +
          "'rdf:type' === 'v-ui:DatatypePropertySpecification' || " +
          "'rdf:type' === 'v-ui:ObjectPropertySpecification'";

      var result = {};
      var ontology_uris = query({ ticket: veda.ticket, query: q, limit: 10000 }).result;
      var ontology_individuals = get_individuals(veda.ticket, ontology_uris);
      ontology_individuals.map( function (item) {
        result[ item["@"] ] = item;
      });
      return result;
    }

  };

});
