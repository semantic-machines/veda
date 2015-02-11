// Console Presenter

veda.Module(function GraphPresenter(veda) { "use strict";

	var container = $("#main");
	
	veda.on("load:graph", function (params) {
		
		function addNode (individual, opts) {
			if ( nodes.get(individual.id) === null ) {
				var node = {
					id: individual.id,
					label: individual["rdf:type"][0]["rdfs:label"][0] + ": \n" + (individual["rdfs:label"] && individual["rdfs:label"][0] ? individual["rdfs:label"][0] : individual.id),
					individual: individual,
				};
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

		function addInLinks (id) {
			var s = new veda.SearchModel("'*'=='" + id + "'", $("<div>"));
			var results = [];
			var resultsEdges = [];
			Object.getOwnPropertyNames(s.results).map(function (uri) {
				var res = s.results[uri];
				var res_id = res["id"];
				var res_label = res["rdf:type"][0]["rdfs:label"][0] + ": \n" + (res["rdfs:label"] && res["rdfs:label"][0] ? res["rdfs:label"][0] : id);
				if (nodes.get(res_id) === null) {
					results.push({id: res_id, label: res_label, individual: res});
				}
				var to = id; 
				var from = res_id;
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
								resultsEdges.push({from: from, to: to, label: label});
							}
						}
					}) 
				});
			});
			nodes.add(results);
			edges.add(resultsEdges);
		}

		// Event handlers
		function onSelect (selected) {
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
			addOutLinks(selected.nodes[0]);
		}

		container.empty();
		var uri = params.length ? params[0] : undefined;
		var root = new veda.IndividualModel(uri);
		var nodes = new vis.DataSet(), edges = new vis.DataSet();
		var body = $("body");
		
		addNode(root, {group: "root"});
		addOutLinks(root.id);
		
		// Create a network
		var data = {
			nodes: nodes,
			edges: edges,
		};
		var options = {
			width: "100%",
			height: "800px",
			nodes: {
				shape: "box"
			},
			edges: {
				style: "arrow",
				arrowScaleFactor: 0.7
			},
			physics: {
				/*barnesHut: {
					enabled: true,
					gravitationalConstant: -2000,
					centralGravity: 0.1,
					springLength: 95,
					springConstant: 0.04,
					damping: 0.09
				},*/
				/*repulsion: {
					centralGravity: 0.1,
					springLength: 50,
					springConstant: 0.05,
					nodeDistance: 100,
					damping: 0.09
				},*/
				hierarchicalRepulsion: {
					centralGravity: 0.5,
					springLength: 150,
					springConstant: 0.01,
					nodeDistance: 300,
					damping: 0.09
				}
			},
		};
		var network = new vis.Network(container.get(0), data, options);

		// Add event listeners
		network.on("doubleClick", onDoubleClick);
		network.on("select", onSelect);
		
	});

});
