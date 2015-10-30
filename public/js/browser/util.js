// Browser-side utility functions

veda.Module(function Util(veda) { "use strict";
	
	veda.Util = veda.Util || {};

	// Escape function for css (jQuery) selectors
	veda.Util.escape4$ = function (str) {
		if (str) return str.replace(/([ #;?%&,.+*~\':"!^$[\]()=>|\/@])/g,'\\$1');
		return str;
	};

	veda.Util.guid = function () {
	  function s4() {
		return Math.floor((1 + Math.random()) * 0x10000)
				   .toString(16)
				   .substring(1);
	  }
	  return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
			 s4() + '-' + s4() + s4() + s4();
	};

	veda.Util.genUri = function () {
	    return 'd:a' + veda.Util.guid();
	};

	veda.Util.construct = function (constr, args) {
		function F() {
			return constr.apply(this, args);
		}
		F.prototype = constr.prototype;
		return new F();
	};
	
	function isInteger(n) { return n % 1 === 0; };
	
	function zeroPref(n) {
		return n > 9 ? n : "0" + n;
	};
	
	veda.Util.formatDate = function (date) {
		var day = date.getDate(),
			month = date.getMonth() + 1,
			year = date.getFullYear(),
			hours = date.getHours(),
			mins = date.getMinutes(), 
			secs = date.getSeconds(),
			fdate, ftime;
		month = zeroPref(month); day = zeroPref(day);
		hours = zeroPref(hours); mins = zeroPref(mins); secs = zeroPref(secs);
		fdate = [day, month, year].join(".");
		ftime = [hours, mins, secs].join(":");
		if (ftime === "00:00:00") return fdate;
		return [fdate, ftime].join(" ");
	};
	veda.Util.formatNumber = function (n) {
		return (n+"").replace(/.(?=(?:[0-9]{3})+\b)/g, '$& ');
	};
	
	veda.Util.exportTTL = function (individualList) {
		var s = new veda.SearchModel("'rdf:type'=='owl:Ontology'", null);
		var prefixes = {};
		prefixes["dc"] = "http://purl.org/dc/elements/1.1/";
		prefixes["grddl"] = "http://www.w3.org/2003/g/data-view#";
		Object.getOwnPropertyNames(s.results).map( function (res_id) {
			var res = s.results[res_id];
			prefixes[res_id.substring(0,res_id.length-1)] = res["v-s:fullUrl"][0].toString();
		});
		var writer = N3.Writer({ prefixes: prefixes });
		individualList.each(function (individual) {
			var triple = {};
			if (individual.id.indexOf(":") == individual.id.length-1) {
				triple.subject = prefixes[individual.id.substring(0, individual.id.length - 1)];
			} else {
				triple.subject = N3.Util.expandPrefixedName(individual.id, prefixes);
			}
			Object.getOwnPropertyNames(individual.properties).map(function (property_uri) {
				triple.predicate = N3.Util.expandPrefixedName(property_uri, prefixes);
				individual[property_uri].map(function (value) {
					if (value instanceof Number || typeof value === "number" ) {
						triple.object = isInteger(value.valueOf()) ? '"' + value.valueOf() + '"^^' + N3.Util.expandPrefixedName('xsd:integer', prefixes) : '"' + value.valueOf() + '"^^' + N3.Util.expandPrefixedName('xsd:decimal', prefixes);
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
	};
	
	veda.Util.applyTransform = function (individualList, transform) {
		return transformation(null, individualList, transform, null, null);
	};
	
	veda.Util.forSubIndividual = function (net, property, id, func) {
		if (net[property]===undefined) return;
		net[property].forEach(function(el) {
			if (el.id == id) {
				func(el);
			}
		});
	};

	veda.Util.removeSubIndividual = function (net, property, id) {
		if (net[property]===undefined) return undefined;
		return net[property].filter( function (item) {
			return item.id !== id; 
		});
	};
	
	/* 
	 * from http://stackoverflow.com/questions/27266550/how-to-flatten-nested-array-in-javascript
	 * by http://stackoverflow.com/users/2389720/aduch
	 * 
	 * This is done in a linear time O(n) without recursion
	 * memory complexity is O(1) or O(n) if mutable param is set to false
	 */
	veda.Util.flatten = function (array, mutable) {
		var toString = Object.prototype.toString;
		var arrayTypeStr = '[object Array]';

		var result = [];
		var nodes = (mutable && array) || array.slice();
		var node;

		if (!array.length) {
			return result;
		}

		node = nodes.pop();

		do {
			if (toString.call(node) === arrayTypeStr) {
				nodes.push.apply(nodes, node);
			} else {
				result.push(node);
			}
		} while (nodes.length && (node = nodes.pop()) !== undefined);

		result.reverse(); // we reverse result to restore the original order
		return result;
	};
	
	veda.Util.queryFromIndividual = function (individual) {
		// Serialize individual as search query
		var query;
		var allProps = Object.getOwnPropertyNames(individual.properties)
			.map(function (property_uri) {
				var property = individual.properties[property_uri];
				var values = individual[property_uri].filter(function(item){return !!item && !!item.valueOf();});
				// Filter rdfs:Resource type
				if (property_uri === "rdf:type") { 
					values = individual[property_uri].filter(function(item){ return item.id !== "rdfs:Resource" });
				}
				var oneProp;
				switch (property["rdfs:range"][0].id) {
					case "xsd:integer": 
					case "xsd:nonNegativeInteger":
					case "xsd:decimal":
						oneProp =
							values.length === 1 ? "'" + property_uri + "'==[" + values[0] + "," + values[0] + "]" :
							values.length > 1 ? "'" + property_uri + "'==[" + values[0] + "," + values[values.length-1] + "]" :
							undefined;
						break;
					case "xsd:dateTime": 
						oneProp =
							values.length === 1 ? "'" + property_uri + "'==[" + values[0].toISOString().substring(0,19) + "," + values[0].toISOString().substring(0,19) + "]" :
							values.length > 1 ? "'" + property_uri + "'==[" + values[0].toISOString().substring(0,19) + "," + values[values.length-1].toISOString().substring(0,19) + "]" :
							undefined;
						break;
					case "xsd:boolean": 
					case "xsd:string": 
					case "rdfs:Literal": 
						oneProp = values
							.filter(function(item){return !!item && !!item.valueOf();})
							.map( function (value) {
								return "'" + property_uri + "'=='" + value + "'";
							})
							.join("||");
						break;
					default:
						oneProp = values
							.filter( function (value) {
								return value instanceof veda.IndividualModel;
							})
							.map( function (value) {
								return "'" + property_uri + "'=='" + value.id + "'";
							})
							.join("||");
						break;
				}
				return oneProp ? "(" + oneProp + ")" : undefined;
			})
			.filter(function(item){return !!item;})
			.join("&&");
		query = allProps ? "(" + allProps + ")" : undefined;
		return query;
	}

});
