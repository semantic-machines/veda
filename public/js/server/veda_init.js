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

  veda.ticket = get_env_str_var('$ticket');

  veda.Module({/* configuration object */});

  var ontology = new veda.OntologyModel();
  ontology.then(function (ontology) {
    veda.ontology = ontology;
  });

  veda.user = new veda.UserModel("cfg:Administrator");

})(veda);
