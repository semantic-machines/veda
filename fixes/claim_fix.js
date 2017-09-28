veda.Util.processQuery("'rdf:type'==='mnd-s:Claim' && 'v-s:datePlan'==[1500000000000, 1600000000000]", 10000, 100, 100, function (claim_uri) {
  try {
    var claim = get_individual(veda.ticket, claim_uri);
    claim['v-s:datePlan'] = [
      {
        type: "Datetime",
        data: new Date( claim['v-s:datePlan'][0].data )
      }
    ];
    console.log( claim_uri, claim['v-s:datePlan'][0].data );
    put_individual(veda.ticket, claim);
  } catch (err) {
    console.log(err);
  }
});

veda.Util.processQuery("'*'=='d:mondi_department_50001654' || '*'=='d:mondi_department_50000001'", 10000, 100, 100, function (uri) {
  try {
    var individual = get_individual(veda.ticket, uri);
    //console.log( "before", individual );
    var str = JSON.stringify(individual);
    str = str.replace(/(d:mondi_department_50001654|d:mondi_department_50000001)/g, "d:org_RU1121003135");
    individual = JSON.parse(str);
    //console.log( "after", individual );
    put_individual(veda.ticket, individual);
  } catch (err) {
    console.log(err);
  }
});
