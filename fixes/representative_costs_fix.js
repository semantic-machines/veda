// mnd-s:RepresentativeCosts

veda.Util.processQuery("'rdf:type'==='mnd-s:RepresentativeCosts'", 10000, 100, 100, function (uri) {
  try {
    var costs = new veda.IndividualModel(uri);
    if ( costs.hasValue("v-s:member") ) {
      costs["v-s:participant"] = costs["v-s:member"];
      costs["v-s:member"] = [];
      //console.log("participant = %s | member = ", JSON.stringify(costs.properties["v-s:participant"]), JSON.stringify(costs.properties["v-s:member"]));
      //console.log( JSON.stringify(costs.properties) );
      //costs.save();
    }
  } catch (err) {
    console.log(err);
  }
});
