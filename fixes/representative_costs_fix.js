// mnd-s:RepresentativeCosts

var costs_array = [];

veda.Util.processQuery("'rdf:type'==='mnd-s:RepresentativeCosts'", 10000, 100, 100, function (uri) {
  try {
    var costs = new veda.IndividualModel(uri);
    if ( costs.hasValue("v-s:member") ) {
      costs["v-s:participant"] = costs["v-s:member"];
      costs["v-s:member"] = [];
      var json = JSON.stringify(costs.properties);
      costs_array.push(json);
      costs.save();
    }
  } catch (err) {
    console.log(err);
  }
});
