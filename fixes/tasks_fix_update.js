//update tasks

var q = query({
  ticket: veda.ticket,
  query: "'rdf:type'==='v-wf:DecisionForm' && 'v-wf:isCompleted' == 'false'",
  top: 100000,
  limit: 100000
});

console.log("tasks count", q.estimated);

var tasks = get_individuals(veda.ticket, q.result);

var limit = 10;

tasks.map(function (task) {
  if (!task["v-wf:to"]) return;
  var receiver_uri = task["v-wf:to"][0].data;
  var receiver = get_individual(veda.ticket, receiver_uri);
  if (receiver["v-s:deleted"] && receiver["v-s:deleted"][0].data === true) {
    if (--limit < 0) { return; }
    try {

      // receiver = deleted position
      if (receiver["rdf:type"][0].data === "v-s:Position") {

        var apps = query({
          ticket: veda.ticket,
          query: "('rdf:type'==='v-s:Appointment' && 'v-s:occupation' == '" + receiver["@"] + "') || ('rdf:type'==='v-s:Appointment' && 'v-s:deleted' == 'true' && 'v-s:occupation' == '" + receiver["@"] + "')",
          top: 100000,
          limit: 100000
        }).result;

        var persons_uris = [].concat(
          apps.map(function (app) {
            return app["v-s:employee"][0].data;
          })
        ).filter( function (i) { return !!i; });

        var person_uri = persons_uris[0];

        var act_apps = query({
          ticket: veda.ticket,
          query: "('rdf:type'==='v-s:Appointment' && 'v-s:employee' == '" + person_uri + "')",
          top: 100000,
          limit: 100000
        }).result;

        var act_app_uri = act_apps[0];
        var act_app = get_individual(veda.ticket, act_app_uri);
        var act_pos_uri = act_app["v-s:occupation"][0].data;

        // assign task & add right to task for active position
        task["v-wf:to"][0].data = act_pos_uri;
        //addRight(veda.ticket, [can_read], act_pos_uri, task["@"]);

        // assign task & add right to task's document for active position
        var doc_uri = task["v-wf:onDocument"] && task["v-wf:onDocument"][0].data;
        if (doc_uri) {
          //addRight(veda.ticket, [can_read], act_pos_uri, doc_uri);
        }

        // update task
        //put_individual(veda.ticket, task);
      }

    } catch (ex) {
      console.log("deleted receiver error:", receiver["@"], ex);
    }

  }
});


// =====================

var del_positions_uris = query({
  ticket: veda.ticket,
  query: "'rdf:type'==='v-s:Position' && 'v-s:deleted' == 'true'",
  top: 100000,
  limit: 100000
}).result;

console.log("del_positions", del_positions_uris.length);

var del_positions = get_individuals(veda.ticket, del_positions_uris);

var no_app = 0;

del_positions.map(function (del_pos) {
  /*var tasks = query({
    ticket: veda.ticket,
    query: "'rdf:type'==='v-wf:DecisionForm' && 'v-wf:isCompleted' == 'false' && 'v-wf:to' == '" + del_pos["@"] + "'",
    top: 100000,
    limit: 100000
  }).result;

  if (tasks.length === 0) return;*/

  var apps_to_del_position = query({
    ticket: veda.ticket,
    query: "('rdf:type'==='v-s:Appointment' && 'v-s:deleted' == 'true' && 'v-s:occupation' == '" + del_pos["@"] + "') || ('rdf:type'==='v-s:Appointment' && 'v-s:occupation' == '" + del_pos["@"] + "')",
    top: 100000,
    limit: 100000
  }).result;

  if (!apps_to_del_position.length) {
    console.log(++no_app, "no app for position", del_pos["@"]);
  }

});
















