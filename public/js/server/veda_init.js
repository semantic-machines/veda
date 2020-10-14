;(function (veda) {

  var console = {
    log: print,
    error: print,
    info: print,
    time: function (timer) {
      this[timer] = new Date();
    },
    timeEnd: function (timer) {
      var delta = new Date() - this[timer];
      this.log(timer, delta, "msec");
    }
  };
  this.console = console;

  veda.Module({/* configuration object */});

  veda.ticket = get_env_str_var('$ticket');

  veda.init("cfg:VedaSystem");

  console.log("user:", veda.user.id, "| ticket:", veda.ticket);

  console.log("query with 2 params:", JSON.stringify(query(veda.ticket, "'rdf:type' == 'owl:Ontology'")));

  console.log("query with 7 params:", JSON.stringify(query(veda.ticket, "'rdf:type' == 'owl:Ontology'", undefined, undefined, undefined, undefined, undefined)));

})(veda);
