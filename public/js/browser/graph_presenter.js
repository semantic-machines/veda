// Console Presenter

veda.Module(function GraphPresenter(veda) { "use strict";

	var container = $("#main");
	var tmpl = $( $("#graph-template").html() );
	
	veda.on("load:graph", function (params) {
		
		function addNode (individual, opts) {
			if ( nodes.get(individual.id) === null ) {
				var node = {
					id: individual.id,
					label: individual["rdf:type"][0]["rdfs:label"][0] + ": \n" + (individual["rdfs:label"] && individual["rdfs:label"][0] ? individual["rdfs:label"][0] : individual.id),
					individual: individual,
				};
				if (individual["rdf:type"][0]) {
					switch ( individual["rdf:type"][0].id ) {
						case "rdfs:Class" :
						case "owl:Class" :
							node.group = "type";
							break
						case "rdf:Property" :
						case "owl:ObjectProperty" :
							node.group = "objectProperty";
							break
						case "owl:DatatypeProperty" :
						case "owl:OntologyProperty" :
						case "owl:AnnotationProperty" :
							node.group = "datatypeProperty";
							break
						case "v-ui:ClassTemplate" :
							node.group = "template";
							break
						case "v-ui:PropertySpecification" :
							node.group = "specification";
							break
						case "owl:Ontology" :
							node.group = "ontology";
							break
						default :
							node.group = "individual";
							break
					}
				}
				$.extend(node, opts);
				nodes.add ([ node ]);
			}
		}
		
		function addOutLinks (id) {
			var individual = nodes.get(id).individual;
			Object.getOwnPropertyNames(individual.properties).map(function (property_uri) {
				var values = individual[property_uri];
				values.map(function (value) {
					if (value instanceof veda.IndividualModel && value.id != individual.id) {
						addNode(value);
						var from = individual.id;
						var to = value.id;
						var label = veda.ontology[property_uri]["rdfs:label"][0].toString();
						var options = {
							filter: function (item) {
								return  item.from == from && 
										item.to == to &&
										item.label.toString() == label
							}
						}
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
		};

		function addInLinks (id, query) {
			var q = query || "'*'=='{id}'";
			q = q.replace("{id}", id);
			var s = new veda.SearchModel(q, $("<div>"));
			Object.getOwnPropertyNames(s.results).map(function (uri) {
				var res = s.results[uri];
				addNode(res);
				var to = id; 
				var from = res.id;
				Object.getOwnPropertyNames(res.properties).map(function (property_uri) {
					res[property_uri].map(function (item) {
						if (item instanceof veda.IndividualModel && item.id == to) {
							var label = veda.ontology[property_uri]["rdfs:label"][0];
							var options = {
								filter: function (item) {
									return  item.from == from && 
											item.to == to &&
											item.label.toString() == label
								}
							}
							if ( !edges.get(options).length ) {
								edges.add([{from: from, to: to, label: label}]);
							}
						}
					}) 
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
			var modal = $("#graph-modal", container).modal();
			var modalBody = $(".modal-body", modal);
			var modalTitle = $(".modal-title", modal);
			modalTitle.text(nodes.get(id).label);
			new veda.DocumentModel(id, modalBody);
			/*riot.route("#/document/" + selected.nodes[0]);*/
			//addOutLinks(selected.nodes[0]);
		}

		var uri = params.length ? params[0] : undefined;
		var root = new veda.IndividualModel(uri);
		var nodes = new vis.DataSet(), edges = new vis.DataSet();
		var body = $("body");
		var select = {nodes: [], edges: []};
		container.empty();
		container.prepend( tmpl );
		var graph = $("#graph", container);
		
		function isInteger (n) { return n % 1 === 0; }
		
		var exportBtn = $("#export-ttl", container).click(function () {
			var s = new veda.SearchModel("'rdf:type'=='owl:Ontology'", $("<div>"));
			var prefixes = {};
			Object.getOwnPropertyNames(s.results).map( function (res_id) {
				var res = s.results[res_id];
				prefixes[res_id.substring(0,res_id.length-1)] = res["v-s:fullUrl"][0].toString() + "#";
			});
			console.log(prefixes);
			var writer = N3.Writer({ prefixes: prefixes });
			nodes.get().map(function (node) {
				var individual = node.individual;
				var triple = {};
				triple.subject = N3.Util.expandPrefixedName(individual.id, prefixes);
				Object.getOwnPropertyNames(individual.properties).map(function (property_uri) {
					triple.predicate = N3.Util.expandPrefixedName(property_uri, prefixes);
					individual[property_uri].map(function (value) {
						if (value instanceof Number || typeof value === "number" ) {
							triple.object = isInteger(value.valueOf()) ? '"' + value.valueOf() + '"^^' + N3.Util.expandPrefixedName('xsd:integer', prefixes) : '"' + value.valueOf() + '"^^' + N3.Util.expandPrefixedName('xsd:decimal', prefixes);
							console.log(triple.object);
						} else if (value instanceof Boolean || typeof value === "boolean") {
							triple.object = '"' + value.valueOf() + '"^^' + N3.Util.expandPrefixedName("xsd:boolean", prefixes);
						} else if (value instanceof String || typeof value === "string") {
							triple.object = value.language ? '"' + value.valueOf() + '"@' + value.language.toLowerCase() : '"' + value.valueOf() + '"^^' + N3.Util.expandPrefixedName("xsd:string", prefixes);
						} else if (value instanceof Date) {
							triple.object = '"' + value.toISOString() + '"^^' + N3.Util.expandPrefixedName("xsd:dateTime", prefixes);
						} else if (value instanceof veda.IndividualModel) {
							if (value.id.indexOf(":") == value.id.length-1) {
								triple.object = prefixes[value.id.substring(0, value.id.length - 1)];
							} else {
								triple.object = N3.Util.expandPrefixedName(value.id, prefixes);
							}
						}
						writer.addTriple(triple);
					});
				});
			});
			writer.end(function (error, result) { 
				var blob = new Blob([result], {type: "text/plain;charset=utf-8"});
				saveAs(blob, "exported_graph.ttl");
			});
		});

		graph.contextmenu({
			target: $("#individual-context-menu", container),
			before: function (e, element) {
				if (!select.nodes.length) return false;
				var id = select.nodes[0];
				var node = nodes.get(id);
				switch (node.group) {
					case "type": this.setMenu($("#class-context-menu", container)); break
					case "ontology": this.setMenu($("#ontology-context-menu", container)); break
					case "property": this.setMenu($("#property-context-menu", container)); break
					case "template": this.setMenu($("#template-context-menu", container)); break
					case "specification": this.setMenu($("#specification-context-menu", container)); break
					default: this.setMenu($("#individual-context-menu", container)); break
				}
				return true;
			},
			onItem: function (context, e) {
				var id = select.nodes[0];
				switch (e.target.id) {
					case "out-links" : addOutLinks( id ); break
					case "in-links" : addInLinks( id ); break
					case "delete" : 
						nodes.remove(select.nodes); 
						edges.remove(select.edges);
						select.nodes = select.edges = [];
					break
					case "delete-with-out" : 
						deleteWithOutLinks (id);
						select.nodes = select.edges = [];
					break
					case "delete-with-in" : 
						deleteWithInLinks (id);
						select.nodes = select.edges = [];
					break
					case "class-individuals" : addInLinks( id, "'rdf:type'=='{id}'" ); break
					case "class-properties" : addInLinks( id, "'rdfs:domain'=='{id}'"); break
					case "class-templates" : addInLinks( id, "'rdf:type'=='v-ui:ClassTemplate'&&'v-ui:forClass'=='{id}'" ); break
					case "class-specifications" : addInLinks( id, "'rdf:type'=='v-ui:PropertySpecification'&&'v-ui:forClass'=='{id}'" ); break
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
		
		var height = ( $("#copyright").offset().top - graph.offset().top ) + "px";
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
				type: {
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
