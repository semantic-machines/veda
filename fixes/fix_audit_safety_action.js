// Audit Safety Actions

var actions_uris = query(veda.ticket, "'rdf:type'==='mnd-s:AuditSafetyAction' && 'v-s:registrationNumber'=='undefined*'", undefined, undefined, undefined, undefined, 10000);

var actions = get_individuals(veda.ticket, actions_uris);

var limit = 10000;

actions.map(function (action_json, index) {
  if (index > limit) return;
  var action = new veda.IndividualModel(action_json);
  var audit = action.hasValue("v-s:parent") ? action["v-s:parent"][0] : undefined ;
  if ( audit ) {
    audit["v-s:hasAction"].map(function (activity, index) {
      activity['v-s:registrationNumber'] = [ audit['v-s:registrationNumber'][0] + '.' + (index + 1) ];
      console.log( activity['v-s:registrationNumber'][0].toString() );
      //activity.save();
    });
  }
});



// Meeting Actions

var actions_uris = query(veda.ticket, "'rdf:type'==='mnd-s:MeetingAction'", undefined, undefined, undefined, undefined, 10000);

var actions = get_individuals(veda.ticket, actions_uris);

var limit = 10000;

var wrongCount = 0;

actions.map(function (action_json, index) {
  if (index > limit) return;
  var action = new veda.IndividualModel(action_json);
  var wrongNumber = action.hasValue("v-s:registrationNumber") && action["v-s:registrationNumber"][0].toString().indexOf(".") < 0;
  if ( wrongNumber ) {
    wrongCount++;
    var meeting = action.hasValue("v-s:parent") ? action["v-s:parent"][0] : undefined ;
    if ( meeting ) {
      meeting["v-s:hasAction"].map(function (activity, idx) {
        activity['v-s:registrationNumber'] = [ meeting['v-s:registrationNumber'][0] + '.' + (idx + 1) ];
        console.log( activity['v-s:registrationNumber'][0].toString() );
        //activity.save();
      });
    }
    //console.log( action["v-s:registrationNumber"][0].toString(), action.id );
  }
});

console.log( "wrongCount =", wrongCount );




// Замена идентификаторов в mnd-s:AuditSafetyAction

var actions_uris = query(veda.ticket, "'rdf:type'==='mnd-s:AuditSafetyAction' && 'mnd-s:hasAuditSafetyObservationKind'=='d:d337c84312e8645e79a0fc1f7f46bc7da'", undefined, undefined, undefined, undefined, 10000);

var actions = get_individuals(veda.ticket, actions_uris);

var count = 0;

var limit = 1;

actions.map(function (action_json, index) {
  //if (index > limit) { return }
  count++;
  action_json["mnd-s:hasAuditSafetyObservationKind"] = [{type: "Uri", data: "d:337c84312e8645e79a0fc1f7f46bc7da"}];
  console.log(action_json["@"]);
  //put_individual(veda.ticket, action_json);
});

console.log("count =", count);




// Замена идентификаторов в mnd-s:AuditSafetyAction

var q = "'rdf:type'==='mnd-s:AuditSafetyAction' && (\
'mnd-s:hasAuditSafetyTheme'=='d:d337c84312e8645e79a0fc1f7f46bc7da' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:d7ec426a0357d41de9912352e82cb24f3' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:db312d9a414dd400292f947a96664b5f7' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:dd458c79d74194b2686abc89022b75da1' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:dff91c3ed2f8d4abe9fda2d380305bdfd' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:df72bbb4d743441299063b0f5a117f555' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:d9bf8a04ab386440d901570508dd29dcd' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:d41b7380b6c054abf9df82c31b3b996ec' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:dd147170843d14618a8018677bf19c5d2' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:dca1e7e3f7bb84465a0ba373db5916612' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:db9bc6c11becd4140ba49f0d08599403e' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:d6600f054f71841529146e6c07984eb35' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:d4441404d540547a497d24abc121cc0bd' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:db656bd85313d4bd9bbce5ee109dc08dc' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:df89b085799354ddf88486e313cecf94c' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:d8bad9adc-db3b-4d0f-b0a8-9a7729d49369' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:dde58386cc48b44bb827626724d59b21f' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:d0f2a399c-009e-410c-ba6a-10ebf0825fad' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:d3b5863e5-ac37-4b4b-afe3-8f2182511220' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:d2ac7cec5-bec0-4246-a5d3-016abab1b66b' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:dca398941-48a9-4267-8fbd-c04b9c9be3fd' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:dca112b10aadd4e5db038d21c368b7213' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:d0c96ac0c4ae243d9a8a0431fe0468b75' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:d7df52b49e00b4106981687b3a89f2730' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:df45520ad887041de9381f90e699b9c97' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:d3364bb2bc630413caae03449b381e744' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:dba6cc096a7a14ad2ad719da95a1ac906' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:dc9846bdd87a549e1ba428e0dd8bb7237' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:d8d22c43b037f4447b9777c222a728c3a' \
|| 'mnd-s:hasAuditSafetyTheme'=='d:db8b5376e284b46ab97bab4a1b8008ffb')";

var actions_uris = query(veda.ticket, q, undefined, undefined, undefined, undefined, 10000);

var actions = get_individuals(veda.ticket, actions_uris);

var count = 0;

var limit = 10;

actions.map(function (action_json, index) {
  if (index >= limit) { return }
  count++;
  var value = action_json["mnd-s:hasAuditSafetyTheme"][0].data;
  action_json["mnd-s:hasAuditSafetyTheme"][0].data = "d:" + value.substring(3);
  console.log(action_json);
  //put_individual(veda.ticket, action_json);
});

console.log("count =", count);







// Audit Safety Actions Priority Level

var actions_uris = query({
  ticket: veda.ticket,
  query: "'rdf:type'==='mnd-s:AuditSafetyAction' && ( 'v-s:hasPriorityLevel'=='d:2cdbc09fbd4a400eb24af7f28ce258ab' || 'v-s:hasPriorityLevel'=='d:4325ea04d1ab4eeaaed3dde0de8df52a' || 'v-s:hasPriorityLevel'=='d:e63d530ac24d45c599cf0e9730b6ee0b' )",
  limit: 100000,
  top: 100000
}).result;

console.log("Total", actions_uris.length);

var errors = 0;

actions_uris.map(function (action_uri) {
  try {
    var action = get_individual(veda.ticket, action_uri);
    var level = action["v-s:hasPriorityLevel"][0].data;
    if (level === "d:2cdbc09fbd4a400eb24af7f28ce258ab") {
      action["v-s:hasPriorityLevel"][0].data = "d:13545e8b279170bf558caf04382077d3";
    } else if (level === "d:4325ea04d1ab4eeaaed3dde0de8df52a") {
      action["v-s:hasPriorityLevel"][0].data = "d:15e1d21a6bf8fdc94b693fadd960f71e";
    } else if (level === "d:e63d530ac24d45c599cf0e9730b6ee0b") {
      action["v-s:hasPriorityLevel"][0].data = "d:15e1d21a6bf8fdc94b693fadd960f71e";
    }
    put_individual(veda.ticket, action);
  } catch (error) {
    console.log(++errors, "Error", error, "action_uri", action_uri);
  }
});


// Re-index mnd-s:ActFailureAction
veda.Util.processQuery("'rdf:type'==='mnd-s:ActFailureAction'", 1000, 100, 1000, function (action_uri) {
  try {
    var action = get_individual(veda.ticket, action_uri);
    put_individual(veda.ticket, action);
  } catch (err) {
    console.log("Error.", "| action_uri", action_uri);
  }
});




