// delegated appointments labels fix

var q = query({
  ticket: veda.ticket,
  query: "('rdf:type'==='v-s:Appointment' || ('rdf:type'==='v-s:Appointment' && 'v-s:deleted' == 'true') ) && ('v-s:origin' == 'user' || 'v-s:origin' == 'admin')",
  top: 10000,
  limit: 10000
});

var apps = get_individuals( veda.ticket, q.result );

var cnt = 0;

apps.map(function (app) {
  if ( app["rdfs:label"][0].data.indexOf("d:delegated") !== 0 ) { return; }

  console.log(++cnt, app["rdfs:label"][0].data );

  var uri = app["@"];
  var appointment_label;

  try {
    var delegate_uri = app["v-s:employee"][0].data;
    var position_uri = app["v-s:occupation"][0].data;
    var delegate = get_individual(veda.ticket, delegate_uri);
    var position = get_individual(veda.ticket, position_uri);
    var delegate_labels = delegate["rdfs:label"];
    var position_labels = position["rdfs:label"];
    var appointment_label = delegate_labels.map(function (item) {
      var lang = item.lang;
      var pos_label = position_labels.map( function (label) {
        return label.lang === lang ? label.data : undefined;
      }).filter( function (val) {
        return val !== undefined;
      });
      item.data = item.data + " : " + pos_label.join(", ") + " (*)";
      return item;
    });
    console.log("new label:", JSON.stringify(appointment_label) );
  } catch (err) {
    console.log(err);
    appointment_label = newStr(uri);
  }
  app["rdfs:label"] = appointment_label;

  put_individual(veda.ticket, app);
});
