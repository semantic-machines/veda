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
    "owl:versionInfo"//,
    //"rdf:value",
    //"rdfs:isDefinedBy",
    //"rdfs:member",
    //"rdfs:seeAlso"
  ];

  veda.OntologyModel = function () {

    // Initialization percentage
    veda.trigger("init:progress", 0);

    //var t1 = new Date();

    var self = this;

    var classes = self.classes = {},
        properties = self.properties = {},
        specifications = self.specifications = {},
        models = self.models = {},
        templates = self.templates = {},
        ontologies = self.ontologies = {},
        datatypes = self.datatypes = {};

    var ontology;
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

    // Initialize individual properties in {veda.IndividualModel.prototype}
    Object.keys(ontology).map(function (property_uri) {
      var individual = ontology[property_uri],
        type = individual["rdf:type"][0].data;
      if (
        ( type === 'rdf:Property'
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
        case "owl:Ontology" :
          ontologies[individual.id] = individual;
          break;
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
        case "rdfs:Datatype" :
          datatypes[individual.id] = individual;
          break;
        case "v-ui:PropertySpecification" :
        case "v-ui:DatatypePropertySpecification" :
        case "v-ui:ObjectPropertySpecification" :
          specifications[individual.id] = individual;
          break;
        case "v-s:ClassModel" :
          models[individual.id] = individual;
          break;
        case "v-ui:ClassTemplate" :
          templates[individual.id] = individual;
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
        _class["rdfs:subClassOf"] = [ classes["rdfs:Resource"] ];
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
      property["rdfs:domain"].map( function ( _class ) {
        _class.domainProperties = _class.domainProperties || {};
        _class.domainProperties[property.id] = property;
      });
    });

    // Initialization percentage
    veda.trigger("init:progress", 60);

    // Process specifications
    Object.keys(specifications).map( function (uri) {
      var spec = specifications[uri];
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

    // Propagate properties, specifications and templates to sub-classes.
    // rdfs:Resource is a top-level class.
    propagate("rdfs:Resource");
    function propagate(class_uri) {
      var _class = classes[class_uri];
      for (var subClass_uri in _class.subClasses) {
        var subClass = _class.subClasses[subClass_uri];
        // Propagate properties
        for (var property_uri in _class.domainProperties) {
          if( !subClass.domainProperties || !subClass.domainProperties[property_uri] ) {
            subClass.domainProperties ?
              subClass.domainProperties[property_uri] = _class.domainProperties[property_uri] :
              subClass.domainProperties = {}, subClass.domainProperties[property_uri] = _class.domainProperties[property_uri];
          }
        }
        // Propagate specs
        for (var property_uri in _class.specsByProps) {
          if ( !subClass.specsByProps || !subClass.specsByProps[property_uri] ) {
            subClass.specsByProps ?
              subClass.specsByProps[property_uri] = _class.specsByProps[property_uri] :
              subClass.specsByProps = {}, subClass.specsByProps[property_uri] = _class.specsByProps[property_uri];
          }
        }
        // Propagate template
        /*if (!subClass.template) {
          subClass.template = _class.template;
        }*/
        propagate(subClass_uri);
      }
    }

    // Initialization percentage
    veda.trigger("init:progress", 90);

    // Initialization percentage
    Object.keys(self).map( function (uri) {
      var individual = self[uri];
      if (!individual.id || individual.id === "cfg:OntoVsn") return;
      individual.init();
      delete self[uri];
    });

    veda.trigger("init:progress", 100);

    //var t2 = new Date();
    //console.log("onto load", (t2-t1)/1000, "sec");

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
          "'rdf:type' === 'v-ui:DatatypePropertySpecification' || " +
          "'rdf:type' === 'v-ui:ObjectPropertySpecification'";

      var result = {};
      get_individuals(veda.ticket, query(veda.ticket, q, undefined, undefined, undefined, undefined, 10000).result).map( function (item) {
        result[ item["@"] ] = item;
      });
      return result;
    }
  };

});
