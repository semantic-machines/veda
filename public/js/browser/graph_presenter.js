// Console Presenter

veda.Module(function GraphPresenter(veda) { "use strict";

	//Get template
	var container = $("#main");
	
	veda.on("load:graph", function (params) {
		
		container.empty();
		
		var uri = params.length ? params[0] : undefined;
		
		var nodes = [], edges = [];
		
		var individual = new veda.IndividualModel(uri);
		nodes.push (
			{
				id: individual.id,
				label: individual["rdfs:label"] && individual["rdfs:label"][0] ? individual["rdfs:label"][0] : individual.id
			}
		);
		
		Object.getOwnPropertyNames(individual.properties).map(function (property_uri) {
			var values = individual[property_uri];
			values.map(function (value) {
				if (value instanceof veda.IndividualModel && value.id != individual.id) {
					nodes.push (
						{
							id: value.id,
							label: value["rdfs:label"] && value["rdfs:label"][0] ? value["rdfs:label"][0] : value.id
						}
					);
					edges.push (
						{
							from: individual.id,
							to: value.id
						}
					);
				}
			});
		});
	
		/*var nodes = [
			{id: 1, label: 'Node 1'},
			{id: 2, label: 'Node 2'},
			{id: 3, label: 'Node 3'},
			{id: 4, label: 'Node 4'},
			{id: 5, label: 'Node 5'}
		];

		// create an array with edges
		var edges = [
			{from: 1, to: 2},
			{from: 1, to: 3},
			{from: 2, to: 4},
			{from: 2, to: 5}
		];
		*/
		
		// create a network
		var data= {
			nodes: nodes,
			edges: edges,
		};
		var options = {
			width: '100%',
			height: '800px'
		};
		
		var network = new vis.Network(container.get(0), data, options);
		
	});

});
