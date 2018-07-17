;(function (veda) {

  veda.ticket = get_env_str_var('$ticket');

  veda.Module({/* configuration object */});

  veda.ontology = new veda.OntologyModel();

  veda.user = new veda.UserModel("cfg:Administrator");

})(veda);
