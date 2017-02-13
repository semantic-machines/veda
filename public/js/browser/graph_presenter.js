// Graph Presenter

veda.Module(function GraphPresenter(veda) { "use strict";

  var tmpl = $("#graph-template").html();

  veda.on("load:graph", function (uri) {

    function addNode (individual, opts) {
      if ( nodes.get(individual.id) === null ) {
        var node = {
          id: individual.id,
          label: (individual["rdf:type"].length ? individual["rdf:type"][0]["rdfs:label"][0] + ": \n" : "") + (individual["rdfs:label"] && individual["rdfs:label"][0] ? individual["rdfs:label"][0] : individual.id),
          individual: individual,
        };
        if (individual["rdf:type"][0]) {
          switch ( individual["rdf:type"][0].id ) {
            case "rdfs:Class" :
            case "owl:Class" :
              node.group = "_class";
              break;
            case "rdf:Property" :
            case "owl:ObjectProperty" :
              node.group = "objectProperty";
              break;
            case "owl:DatatypeProperty" :
            case "owl:OntologyProperty" :
            case "owl:AnnotationProperty" :
              node.group = "datatypeProperty";
              break;
            case "v-ui:ClassTemplate" :
              node.group = "template";
              break;
            case "v-ui:PropertySpecification" :
            case "v-ui:DatatypePropertySpecification" :
            case "v-ui:ObjectPropertySpecification" :
              node.group = "specification";
              break;
            case "owl:Ontology" :
              node.group = "ontology";
              break;
            default :
              node.group = "individual";
              break;
          }
        }
        $.extend(node, opts);
        nodes.add ([ node ]);
      }
    }

    function addOutLinks (id) {
      var individual = nodes.get(id).individual;
      Object.getOwnPropertyNames(individual.properties).map(function (property_uri) {
        if(property_uri === "@") { return }
        var values = individual[property_uri];
        if (!values) return;
        values.map(function (value) {
          if (value instanceof veda.IndividualModel && value.id != individual.id) {
            addNode(value);
            var from = individual.id;
            var to = value.id;
            var label = (new veda.IndividualModel(property_uri))["rdfs:label"].join(", ");
            var options = {
              filter: function (item) {
                return  item.from == from &&
                    item.to == to &&
                    item.label.toString() == label;
              }
            };
            if ( !edges.get(options).length ) {
              edges.add ([
                {
                  from: from,
                  to: to,
                  label: label
                }
              ]);
            }
          }
        });
      });
    }

    function addInLinks (id, query) {
      var q = query || "'*'=='{id}'";
      q = q.replace("{id}", id);
      var s = new veda.SearchModel(q, null);
      Object.getOwnPropertyNames(s.results).map(function (uri) {
        var res = s.results[uri];
        addNode(res);
        var to = id;
        var from = res.id;
        Object.getOwnPropertyNames(res.properties).map(function (property_uri) {
          if(property_uri === "@") { return }
          res[property_uri].map(function (item) {
            if (item instanceof veda.IndividualModel && item.id == to) {
              var label = (new veda.IndividualModel(property_uri))["rdfs:label"].join(", ");
              var options = {
                filter: function (item) {
                  return  item.from == from &&
                      item.to == to &&
                      item.label.toString() == label;
                }
              };
              if ( !edges.get(options).length ) {
                edges.add([{from: from, to: to, label: label}]);
              }
            }
          });
        });
      });
    }

    function deleteWithOutLinks (id) {
      nodes.remove(id);
      var nodesToRemove = [];
      var edgesToRemove = edges.get({
        filter: function (item) {
          if (item.from == id) {
            nodesToRemove.push(item.to);
          }
          return (item.from == id || item.to == id);
        }
      });
      edges.remove(edgesToRemove);
      nodes.remove(nodesToRemove);
    }

    function deleteWithInLinks (id) {
      nodes.remove(id);
      var nodesToRemove = [];
      var edgesToRemove = edges.get({
        filter: function (item) {
          if (item.to == id) {
            nodesToRemove.push(item.from);
          }
          return (item.from == id || item.to == id);
        }
      });
      edges.remove(edgesToRemove);
      nodes.remove(nodesToRemove);
    }

    // Event handlers
    function onSelect (selected) {
      select = selected;
      body.off("keydown");
      body.on("keydown", function (e) {
        if (e.which == 46) {
          nodes.remove(selected.nodes);
          edges.remove(selected.edges);
        }
        if (e.which == 73) {
          addInLinks(selected.nodes[0]);
        }
        if (e.which == 79) {
          addOutLinks(selected.nodes[0]);
        }
        //console.log(e.which);
      });
    }

    function onDoubleClick (selected) {
      if (!selected.nodes.length) return;
      var id = selected.nodes[0];
      var modalTmpl = $("#graph-modal-template").html();
      var modal = $(modalTmpl);
      var modalBody = $(".modal-body", modal);
      var modalTitle = $(".modal-title", modal);
      modalTitle.text(nodes.get(id).label);
      var doc = new veda.IndividualModel(id)
      doc.present(modalBody);
      modal.on("remove", function (e) {
        modal.modal("hide");
      });
      modal.modal();
      $("#main").append(modal);
    }

    var root = new veda.IndividualModel(uri);

    if (!uri) riot.route("#/graph/" + root.id, false);

    var nodes = new vis.DataSet(), edges = new vis.DataSet();
    var body = $("body");
    var select = {nodes: [], edges: []};
    var container = $("#main");
    container.empty();
    container.prepend( $(tmpl) );
    var graph = $("#graph", container);

    // Buttons
    var exportBtn = $("#export-ttl", container).click(function () {
      var list = new veda.IndividualListModel( nodes.get().map(function (item) { return item.individual; }) );
      veda.Util.exportTTL(list);
    });
    var freezeBtn = $("#freeze", container).click(function () {
      network.freezeSimulation = !network.freezeSimulation;
      $("i", this).toggleClass("glyphicon-pause glyphicon-play");
    });

    // Context menu for selected node
    graph.contextmenu({
      target: $("#individual-context-menu", container),
      before: function (e, element) {
        if (!select.nodes.length) return false;
        var id = select.nodes[0];
        var node = nodes.get(id);
        switch (node.group) {
          case "_class": this.setMenu($("#class-context-menu", container)); break;
          case "ontology": this.setMenu($("#ontology-context-menu", container)); break;
          case "datatypeProperty":
          case "objectProperty":
            this.setMenu($("#property-context-menu", container)); break;
          case "template": this.setMenu($("#template-context-menu", container)); break;
          case "specification": this.setMenu($("#specification-context-menu", container)); break;
          default: this.setMenu($("#individual-context-menu", container)); break;
        }
        return true;
      },
      onItem: function (context, e) {
        var id = select.nodes[0];
        switch (e.target.id) {
          case "out-links" : addOutLinks( id ); break;
          case "in-links" : addInLinks( id ); break;
          case "delete" :
            nodes.remove(select.nodes);
            edges.remove(select.edges);
            select.nodes = select.edges = [];
            break;
          case "delete-with-out" :
            deleteWithOutLinks (id);
            select.nodes = select.edges = [];
            break;
          case "delete-with-in" :
            deleteWithInLinks (id);
            select.nodes = select.edges = [];
            break;
          case "class-individuals" : addInLinks( id, "'rdf:type'==='{id}'" ); break;
          case "class-subclasses" : addInLinks( id, "('rdf:type'==='owl:Class'||'rdf:type'==='rdfs:Class')&&'rdfs:subClassOf'==='{id}'" ); break;
          case "class-properties" : addInLinks( id, "'rdfs:domain'==='{id}'"); break;
          case "class-templates" : addInLinks( id, "'rdf:type'==='v-ui:ClassTemplate'&&'v-ui:forClass'==='{id}'" ); break;
          case "class-specifications" :
            addInLinks( id, "('rdf:type'==='v-ui:PropertySpecification'||" +
                    "'rdf:type'==='v-ui:DatatypePropertySpecification'" +
                    "'rdf:type'==='v-ui:ObjectPropertySpecification'" +
                    ")&&'v-ui:forClass'==='{id}'" );
          break;
          case "property-specifications" :
            addInLinks( id, "('rdf:type'==='v-ui:PropertySpecification'||" +
                    "'rdf:type'==='v-ui:DatatypePropertySpecification'" +
                    "'rdf:type'==='v-ui:ObjectPropertySpecification'" +
                    ")&&'v-ui:forProperty'=='{id}'" );
          break;
        }
      }
    });

    // Create a network
    var data = {
      nodes: nodes,
      edges: edges,
    };

    addNode(root);
    addOutLinks(root.id);

    var height = ( $("#copyright").offset().top - graph.offset().top - 50 ) + "px";
    var options = {
      width: "100%",
      height: height,
      nodes: {
        shape: "box"
      },
      edges: {
        style: "arrow",
        arrowScaleFactor: 0.7
      },
      groups: {
        _class: {
          color: {
            border: 'green',
            background: 'lightgreen',
            highlight: {
              border: 'green',
              background: 'lightgreen'
            }
          }
        },
        datatypeProperty: {
          color: {
            border: 'goldenrod',
            background: 'gold',
            highlight: {
              border: 'goldenrod',
              background: 'gold'
            }
          }
        },
        objectProperty: {
          color: {
            border: 'darkorange',
            background: 'orange',
            highlight: {
              border: 'darkorange',
              background: 'orange'
            }
          }
        },
        template: {
          color: {
            border: 'darkviolet',
            background: 'violet',
            highlight: {
              border: 'darkviolet',
              background: 'violet'
            }
          }
        },
        specification: {
          color: {
            border: 'hotpink',
            background: 'lightpink',
            highlight: {
              border: 'hotpink',
              background: 'lightpink'
            }
          }
        },
        ontology: {
          color: {
            border: 'darkgreen',
            background: 'green',
            highlight: {
              border: 'darkgreen',
              background: 'green'
            }
          },
          fontColor: "white"
        },

      },
      physics: {
        barnesHut: {
          enabled: true,
          gravitationalConstant: -4000,
          centralGravity: 0.1,
          springLength: 200,
          springConstant: 0.04,
          damping: 0.09
        },
        /*repulsion: {
          centralGravity: 0.1,
          springLength: 50,
          springConstant: 0.05,
          nodeDistance: 100,
          damping: 0.09
        },*/
        /*hierarchicalRepulsion: {
          centralGravity: 0.5,
          springLength: 150,
          springConstant: 0.01,
          nodeDistance: 300,
          damping: 0.09
        }*/
      },

    };

    var network = new vis.Network(graph.get(0), data, options);

    // Add event listeners
    network.on("doubleClick", onDoubleClick);

    network.on("select", onSelect);
  });

});
