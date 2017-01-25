// Test query

var user_ticket = authenticate("karpovr", Sha256.hash("123")).id;

var admin_ticket = authenticate("karpovrt", Sha256.hash("123")).id;

var user_params = {
  ticket: user_ticket, 
  query: "'rdf:type'==='mnd-s:Meeting'",
  sort: "'v-s:created' asc",
  top: 3,
  from: 0
};

var admin_params = {
  ticket: admin_ticket, 
  query: "'rdf:type'==='mnd-s:Meeting'",
  sort: "'v-s:created' asc"
};

var user_results = veda.query(user_params);

var admin_results = veda.query(admin_params);

console.log("user_results", user_results);

console.log("admin_results", admin_results);

var now = new Date();


var tmpl = {
  "@": "$",
  "rdf:type": [{"type": "Uri","data": "mnd-s:Meeting"}],
  "rdfs:label": [{"lang": "NONE","type": "String","data": "$"}],
  "v-s:agenda": [{"lang": "NONE","type": "String","data": "$"}],
  "v-s:created": [{"type": "Datetime","data": "2017-01-25T19:21:11.000Z"}],
  "v-s:creator": [{"type": "Uri","data": "d:cc513980-a31f-4109-bf36-95e14b2264a0"}],
  "v-s:meetingDecision": [{"lang": "NONE","type": "String","data": "$"}],
  "v-s:theme": [{"lang": "NONE","type": "String","data": "$"}]
}

for (var i = 1; i<=10; i++) {
  var id = "d:TestMeeting" + i;
  var meeting = JSON.parse( JSON.stringify(tmpl).replace(/\$/g, i) );
  put_individual(user_ticket, meeting);
}
