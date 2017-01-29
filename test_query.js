// Meeting

var user = authenticate("karpovr", Sha256.hash("123"));

var admin = authenticate("karpovrt", Sha256.hash("123"));

var meeting_template = '{\
  "@": "d:TestMeeting_$i", \
  "rdf:type": [{ "type": "Uri", "data": "mnd-s:Meeting" }], \
  "rdfs:label": [{ "type": "String", "data": "$i", "lang": "NONE" }], \
  "v-s:agenda": [{ "type": "String", "data": "$i", "lang": "NONE" }], \
  "v-s:created": [{ "type": "Datetime", "data": "2017-01-26T08:57:$i.000Z"}], \
  "v-s:creator": [{ "type": "Uri", "data": "$creator" }], \
  "v-s:meetingDecision": [{ "type": "String", "data": "$i", "lang": "NONE" }], \
  "v-s:registrationNumber": [{ "type": "String", "data": "$i", "lang": "NONE" }], \
  "v-s:theme": [{ "type": "String", "data": "$i", "lang": "RU" }] \
}';

createMeetings(user, 1, 5);
createMeetings(admin, 6, 3);
createMeetings(user, 9, 3);
createMeetings(admin, 12, 9);
createMeetings(user, 21, 5);
createMeetings(admin, 26, 5);
createMeetings(user, 31, 10);
createMeetings(admin, 41, 10);

setTimeout(mkQueries, 3000);

function mkQueries() {
  var q = "'rdf:type'==='mnd-s:Meeting'";
  var s = "'v-s:registrationNumber' asc"

  var params_admin1 = {
    ticket: admin.id,
    query: q,
    sort: s,
    top: 3,
    from: 0
  };
  var results_admin1 = query(params_admin1);
  //console.log("params_admin1", params_admin1, "results_admin1", results_admin1);
  console.log("results_admin1", results_admin1);

  var params_admin2 = {
    ticket: admin.id,
    query: q,
    sort: s,
    top: 10,
    from: 10
  };
  var results_admin2 = query(params_admin2);
  //console.log("params_admin2", params_admin2, "results_admin2", results_admin2);
  console.log("results_admin2", results_admin2);

  var params_user1 = {
    ticket: user.id,
    query: q,
    sort: s,
    top: 6,
    from: 0
  };
  var results_user1 = query(params_user1);
  //console.log("params_user1", params_user1, "results_user1", results_user1);
  console.log("results_user1", results_user1);

  var params_user2 = {
    ticket: user.id,
    query: q,
    sort: s,
    top: 10,
    limit: 10,
    from: 3
  };
  var results_user2 = query(params_user2);
  //console.log("params_user2", params_user2, "results_user2", results_user2);
  console.log("results_user2", results_user2);

}

function createMeetings(creator, start, count) {
  for (var i = start; i < start + count; i++) {
    var meeting = JSON.parse( meeting_template.replace(/\$i/g, i.toString().length === 2 ? i : "0" + i ).replace(/\$creator/g, creator.user_uri) );
    put_individual(creator.id, meeting);
  }
}



// Resource

var user = authenticate("bushenevvt", "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3");
var admin = authenticate("karpovrt", "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3");

var meeting_template = '{\
  "@": "d:QueryTestResource_$i", \
  "rdf:type": [{ "type": "Uri", "data": "rdfs:Resource" }], \
  "v-s:creator": [{ "type": "Uri", "data": "$creator" }], \
  "rdfs:label": [{ "type": "String", "data": "$i", "lang": "NONE" }] \
}';

createMeetings(user, 1, 5);
createMeetings(admin, 6, 3);
createMeetings(user, 9, 3);
createMeetings(admin, 12, 9);

var q = "'rdf:type'==='rdfs:Resource' && '@'=='d:QueryTestResource*'";
var s = "'rdfs:label' asc"

var params_admin1 = {
  ticket: admin.id,
  query: q,
  sort: s,
  top: 3,
  from: 0
};
var results_admin1 = query(params_admin1);
console.log(results_admin1.count === 3 && results_admin1.cursor === 3 && results_admin1.processed === 3, "params_admin1", params_admin1, "results_admin1", results_admin1);
//ok(results_admin1.count === 3 && results_admin1.cursor === 3 && results_admin1.processed === 3);

var params_admin2 = {
  ticket: admin.id,
  query: q,
  sort: s,
  top: 10,
  from: 10
};
var results_admin2 = query(params_admin2);
console.log(results_admin2.count === 10 && results_admin2.cursor === 20 && results_admin2.processed === 10, "params_admin2", params_admin2, "results_admin2", results_admin2);
//ok(results_admin2.count === 10 && results_admin2.cursor === 20 && results_admin2.processed === 10);

var params_user1 = {
  ticket: user.id,
  query: q,
  sort: s,
  top: 6,
  from: 0
};
var results_user1 = query(params_user1);
console.log(results_user1.count === 6 && results_user1.cursor === 9 && results_user1.processed === 9, "params_user1", params_user1, "results_user1", results_user1);
//ok(results_user1.count === 6 && results_user1.cursor === 9 && results_user1.processed === 9);

var params_user2 = {
  ticket: user.id,
  query: q,
  sort: s,
  top: 10,
  limit: 10,
  from: 3
};
var results_user2 = query(params_user2);
console.log(results_user2.count === 5 && results_user2.cursor === 13 && results_user2.processed === 10, "params_user2", params_user2, "results_user2", results_user2);
//ok(results_user2.count === 5 && results_user2.cursor === 13 && results_user2.processed === 10);

function createMeetings(creator, start, count) {
  for (var i = start; i < start + count; i++) {
    var meeting = JSON.parse( meeting_template.replace(/\$i/g, i.toString().length === 2 ? i : "0" + i ).replace(/\$creator/g, creator.user_uri) );
    var res = put_individual(creator.id, meeting);
    wait_module(fulltext_indexer, res.op_id);
  }
}
