// veda.Util.processQuery(query, limit, delta, pause, callback)
veda.Util.processQuery("'rdf:type'==='mnd-s:Pass' && 'mnd-s:passVehicleRegistrationNumber.isExists'==true ", 100000, 1000, 0, function (uri) {
  try {
    var pass = get_individual(veda.ticket, uri);
    var num = pass["mnd-s:passVehicleRegistrationNumber"][0].data;
    var num_replaced = num.replace(/\s*(\d+)\s*/gi, " $1 ");
    pass["mnd-s:passVehicleRegistrationNumber"][0].data = num_replaced;
    put_individual(veda.ticket, pass);
    //console.log(num, "->", num_replaced);
  } catch (err) {
    console.log(err, "Error", "| uri", uri);
  }
});
