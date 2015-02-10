// Console Presenter

veda.Module(function GraphPresenter(veda) { "use strict";

	//Get template
	var container = $("#main");
	
	veda.on("load:graph", function (params) {
		
		container.empty();
		
		var uri = params.length ? params[0] : undefined;
		
		var nodes = new vis.DataSet(), edges = new vis.DataSet();
		
		function expand (uri) {
			var individual = new veda.IndividualModel(uri);
			var a = nodes.get(uri);
			if ( nodes.get(uri) === null ) {
				nodes.add ([
					{
						id: individual.id,
						label: individual["rdf:type"][0]["rdfs:label"][0] + ": \n" + (individual["rdfs:label"] && individual["rdfs:label"][0] ? individual["rdfs:label"][0] : individual.id)
					}
				]);
			}
			Object.getOwnPropertyNames(individual.properties).map(function (property_uri) {
				var values = individual[property_uri];
				values.map(function (value) {
					if (value instanceof veda.IndividualModel && value.id != individual.id) {
						if ( nodes.get(value.id) === null ) {
							nodes.add ([
								{
									id: value.id,
									label: value["rdf:type"][0]["rdfs:label"][0] + ": \n" + (value["rdfs:label"] && value["rdfs:label"][0] ? value["rdfs:label"][0] : value.id)
								}
							]);
						}
						var options = {
							filter: function (item) {
								return  item.from == individual.id && 
										item.to == value.id &&
										item.label.toString() == veda.ontology[property_uri]["rdfs:label"][0].toString()
							}
						}
						var a = edges.get(options);
						if ( !edges.get(options).length ) {
							edges.add ([
								{
									from: individual.id,
									to: value.id,
									label: veda.ontology[property_uri]["rdfs:label"][0]
								}
							]);
						}
					}
				});
			});
		};
		
		expand(uri);
		
		// create a network
		var data= {
			nodes: nodes,
			edges: edges,
		};
		var options = {
			width: "100%",
			height: "800px",
			nodes: {
				shape: "box"
			}, 
			physics: {
				barnesHut: {
					enabled: true,
					gravitationalConstant: -2000,
					centralGravity: 0.1,
					springLength: 150,
					springConstant: 0.04,
					damping: 0.09
				},
			},
		};
		
		var network = new vis.Network(container.get(0), data, options);
		
		function onDoubleClick (properties) {
			properties.nodes.map( function (node) {
				expand(node);
			});
		}
	
		var body = $("body");
		
		function onSelect (properties) {
			body.off("keydown");
			body.on("keydown", function (e) {
				//console.log(e.which);
				if (e.which == 46) {
					nodes.remove(properties.nodes);
					edges.remove(properties.edges);
				}
				if (e.which == 73) {
					var s = new veda.SearchModel("'*'=='" + properties.nodes[0] + "'", $("<div>"));
					var results = [];
					var addEdges = [];
					Object.getOwnPropertyNames(s.results).map(function (uri) {
						var result = s.results[uri];
						var id = result["id"];
						var label = result["rdf:type"][0]["rdfs:label"][0] + ": \n" + (result["rdfs:label"] && result["rdfs:label"][0] ? result["rdfs:label"][0] : id);
						if (nodes.get(result.id) === null) { 
							results.push({id: id, label: label});
							var to = properties.nodes[0];
							var from = id;
							Object.getOwnPropertyNames(result.properties).map(function (property_uri) {
								result[property_uri].map(function (item) {
									if (item instanceof veda.IndividualModel && item.id == to) {
										var label = veda.ontology[property_uri]["rdfs:label"][0];
										addEdges.push({from: from, to: to, label: label});
									}
								}) 
							});
						}
					});
					nodes.add(results);
					edges.add(addEdges);
				}
			});

		}

		// add event listener
		network.on("doubleClick", onDoubleClick);
		
		network.on("select", onSelect);
		
	});

});
