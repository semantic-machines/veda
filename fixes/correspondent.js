// v-s:Correspondent -> mnd-s:Correspondent
// veda.Util.processQuery(query, limit, delta, pause, callback)

veda.Util.processQuery("'rdf:type'==='v-s:Correspondent'", 200000, 100, 100, function (uri) {
  try {
    var correspondent = get_individual(veda.ticket, uri);
    correspondent["rdf:type"] = newUri("mnd-s:Correspondent");
    put_individual(veda.ticket, correspondent);
  } catch (err) {
    console.log(err, "Error", "| uri", uri);
  }
});
