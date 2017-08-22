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


// Restore write permission for UserTaskForms

//veda.Util.processQuery("( 'rdf:type'=='v-wf:DecisionForm' ) && ( 'v-wf:isCompleted'=='false' ) && ( 'v-s:created'==[2017-08-20T21:00:00.000Z,2017-08-22T20:59:59.999Z] )", 100000, 100, 1000, function (uri) {
veda.Util.processQuery("( 'rdf:type'=='v-wf:DecisionForm' ) && ( 'v-wf:isCompleted'=='false' ) && ( 'rdfs:label'=='Доработать')", 100000, 100, 1000, function (uri) {
  try {
    var form = new veda.IndividualModel(uri);
    form.get("v-wf:to").map(function (to) {
      var new_permission = {
        "@": genUri(),
        "rdf:type": [{type:"Uri", data: "v-s:PermissionStatement"}],
        "v-s:permissionObject": [{type:"Uri", data: uri}],
        "v-s:permissionSubject": [{type:"Uri", data: to.id}],
        "v-s:canRead": [{type:"Boolean", data: true}],
        "v-s:canUpdate": [{type:"Boolean", data: true}],
        "v-s:canDelete": [{type:"Boolean", data: true}]
      };
      //put_individual(veda.ticket, new_permission);
      console.log( new_permission["@"] );
    });
  } catch (err) {
    console.log("Error.", uri, err);
  }
});



// UserTaskForm with non-existing decision

var err_count = 0;
var errs_objs = [];
var errs = [];

veda.Util.processQuery("( 'rdf:type'=='v-wf:DecisionForm' ) && ( 'v-wf:isCompleted'=='true' ) && ( 'v-wf:takenDecision.isExists'=='true' ) && ( 'v-s:created'==[2017-08-20T21:00:00.000Z,2017-08-22T20:59:59.999Z] )", 100000, 100, 1000, function (uri) {
  try {
    var form = new veda.IndividualModel(uri);
    var decision = form.get("v-wf:takenDecision")[0];
    if ( !decision || decision["rdf:type"][0].id === "rdfs:Resource" ) {
      errs.push(uri);
      errs_objs.push({form: uri, decision: decision.id});
      console.log(++err_count, "form_uri", uri, "decision_uri", decision.id);
      //form["v-wf:isCompleted"] = [ false ];
      //form["v-wf:takenDecision"] = [];
      //form.save();
    }
  } catch (err) {
    console.log("Error.", uri, err);
  }
});

errs = ["d:hhi944jcyc68wwzvn62hkalu","d:a4mftigyzzhk7cgum8nvo0rzp","d:ntjub9bg59am43bxyuv99if3","d:vsoixpuvind5hbhhl4rqzrml","d:a24onsybic8ula3wxbegdr9ty","d:a30ktptswjwd1gdtn73jhbup9","d:u9eomkaxe7bpc6ah89lhl8h6","d:a948yxdpw0k3wfo7pzuy5lewa","d:j7flhct3gfnyxfzi8cr3lovw","d:fux9froqhgh7vxy76ww1an9x","d:duw0hgog4shfb9xvqif4lqsx","d:p85bnxbg85v37ndg4gicscof","d:a8qufarpbg7skilcucg9gfd6s","d:opclm0reen93qkkafrv54uck","d:fg6al4il2fnsb1vzamqdkgll","d:imfxj3kk8ezg8systkz4l5t0","d:a8tjg0ug0i528e5mhlboffopb"];

errs = ["d:pml16pzymirfvbhp491datw2","d:a4mftigyzzhk7cgum8nvo0rzp","d:a3sbggc84l2i6kvbj80jk6fdk","d:j43034hrfn6zslpxsplaxh99","d:tdpc3ijaeg1mdpgb951de5w5","d:a6ubspjrfjwhxtpaqlpe7dg7q","d:imfxj3kk8ezg8systkz4l5t0"]

