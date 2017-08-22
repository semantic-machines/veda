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


// UserTaskForm with non-existing decision

var err_count = 0;
var errs_objs = [];
var errs = [];

veda.Util.processQuery("( 'rdf:type'=='v-wf:DecisionForm' ) && ( 'v-wf:isCompleted'=='true' ) && ( 'v-wf:takenDecision.isExists'=='true' ) && ( 'v-s:created'==[2017-08-13T21:00:00.000Z,2017-08-21T20:59:59.999Z] )", 100000, 100, 1000, function (uri) {
  try {
    var form = new veda.IndividualModel(uri);
    var decision = form["v-wf:takenDecision"][0];
    if ( decision["rdf:type"][0].id === "rdfs:Resource" ) {
      errs.push(uri);
      errs_objs.push({form: uri, decision: decision.id});
      console.log(++err_count, "form_uri", uri, "decision_uri", decision.id);
    }
  } catch (err) {
    console.log("Error.", uri, err);
  }
});


errs = [
  "d:a4kqji6hisx0sx5jl8jrz3z54",
  "d:fd4j1rlkvnur3bcddguw20eb",
  "d:dzecuonwtscxcdujjdnwkj14",
  "d:lbw3mz3xjwming7az8hb15do",
  "d:sdmzfphw4t55f2y818pfafkd",
  "d:q29lnnzzwfnyzd91sapbakrv",
  "d:nrzpgc40rk2xlrqhjjejdlz0",
  "d:a3xk63r578gdu3re9cqbl5n0d",
  "d:ox5qm69rfskduitunb3rgqym",
  "d:e5ijfhmlzpacc4vfn4kb5pqe",
  "d:n2r3kugohsnzywq0f4yzakz2",
  "d:frhgo9p55tkim5sefxyr8w0n",
  "d:jtzrlsqgvxdy7v6gv63stkm3",
  "d:a5bboa6w1as1hawt5k18x4rb4",
  "d:sw7nm4f2dnmyjtpmu30fap42",
  "d:a9cuichxkmbh83u7q6btcefy9",
  "d:jt9meugj29vkhatteuq1eih2",
  "d:tmvi9kf7giv9niki59mcvgbg",
  "d:kf44vhna8fyahsftisa4gm6z",
  "d:ib3emdhv0pn4dvarbr68bnvx",
  "d:a2och6psiwy0aramkpqnxhosj",
  "d:sz6j18euqocwoy02zd4xb2rp",
  "d:q47rnf4ywu73f43flegg8vjv",
  "d:l9o4byrdv3dsqx46bzsgudji"
];

errs_objs = [
  {"form":"d:a4kqji6hisx0sx5jl8jrz3z54","decision":"d:a7idjk6it1q5qj0hc9vutuleb"},
  {"form":"d:fd4j1rlkvnur3bcddguw20eb","decision":"d:ejm03bkcp2fc3zy79dpkisud"},
  {"form":"d:dzecuonwtscxcdujjdnwkj14","decision":"d:m7gk5ukro8myows6k6bl4us6"},
  {"form":"d:lbw3mz3xjwming7az8hb15do","decision":"d:moe3quiin1lvefemaqki5e7y"},
  {"form":"d:sdmzfphw4t55f2y818pfafkd","decision":"d:yy8mzx7rxn361d3gtnmoq21t"},
  {"form":"d:q29lnnzzwfnyzd91sapbakrv","decision":"d:prvkxtqnrqdhsod8ox6nhitm"},
  {"form":"d:nrzpgc40rk2xlrqhjjejdlz0","decision":"d:bqpff90us3ivi7r4k7toguom"},
  {"form":"d:a3xk63r578gdu3re9cqbl5n0d","decision":"d:a1jrrlbu5qighweekzypq0kv"},
  {"form":"d:ox5qm69rfskduitunb3rgqym","decision":"d:a8n6lzy6u85khdgffil2feub3"},
  {"form":"d:e5ijfhmlzpacc4vfn4kb5pqe","decision":"d:x99wel0dgn04i69tmfnajg2r"},
  {"form":"d:n2r3kugohsnzywq0f4yzakz2","decision":"d:eviy0wrxs9irutrb3lfwx25u"},
  {"form":"d:frhgo9p55tkim5sefxyr8w0n","decision":"d:rrmsnqnnsnj5m8gj0yqk8qy7"},
  {"form":"d:jtzrlsqgvxdy7v6gv63stkm3","decision":"d:orl3bgoe7ft6ipth7hhtd6ne"},
  {"form":"d:a5bboa6w1as1hawt5k18x4rb4","decision":"d:hm0z2s0qfsk1s1l82ps01g73"},
  {"form":"d:sw7nm4f2dnmyjtpmu30fap42","decision":"d:liiv8xg7vw8vacvc7b0hrnh4"},
  {"form":"d:a9cuichxkmbh83u7q6btcefy9","decision":"d:hsuuhmi6o0m60olhd610kg5s"},
  {"form":"d:jt9meugj29vkhatteuq1eih2","decision":"d:tanmk8aq3n43oh50ibr5yf6w"},
  {"form":"d:tmvi9kf7giv9niki59mcvgbg","decision":"d:ohn4jq5l5rf8084elclt4qrl"},
  {"form":"d:kf44vhna8fyahsftisa4gm6z","decision":"d:q71oxwwyvblg4grs77c6zm92"},
  {"form":"d:ib3emdhv0pn4dvarbr68bnvx","decision":"d:a1ojfys4el4agaku650qlll8u"},
  {"form":"d:a2och6psiwy0aramkpqnxhosj","decision":"d:myb19rpcchh7lldmxdlenay3"},
  {"form":"d:sz6j18euqocwoy02zd4xb2rp","decision":"d:l0pdvrh7lob0sjkm9dijdasn"},
  {"form":"d:q47rnf4ywu73f43flegg8vjv","decision":"d:a0kynxpxgvqh55yjnjpakxwwc"},
  {"form":"d:l9o4byrdv3dsqx46bzsgudji","decision":"d:i2i81jr946ytnejs16yutm1y"}
];


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

