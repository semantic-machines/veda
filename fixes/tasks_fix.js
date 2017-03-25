//tasks fixes

var q = query({
  ticket: veda.ticket,
  query: "'rdf:type'==='v-wf:DecisionForm' && 'v-wf:isCompleted' == 'false'",
  top: 10000,
  limit: 10000
});

console.log("tasks count", q.estimated);

var tasks = get_individuals(veda.ticket, q.result);

var receivers_uris = [];

tasks.map(function (task) {
  if (task["v-wf:to"]) {
    receivers_uris.push(task["v-wf:to"][0].data);
  }
});

var receivers = get_individuals(veda.ticket, receivers_uris);

var deleted = [];

receivers.map(function (receiver) {
  if (receiver["v-s:deleted"] && receiver["v-s:deleted"][0].data === true) {
    deleted.push(receiver["@"]);
  }
});

var deleted_unique = unique(deleted);

console.log("deleted", deleted.length, JSON.stringify(deleted));
console.log("deleted_unique", deleted_unique.length, JSON.stringify(deleted_unique));

function unique(arr) {
  var n = {}, r = [];
  for(var i = 0; i < arr.length; i++) {
    if (!n[arr[i]]) {
      n[arr[i]] = true;
      r.push(arr[i]);
    }
  }
  return r;
}


// =================== Simple route start forms

var q = query({
  ticket: veda.ticket,
  query: "'rdf:type'==='s-wf:SimpleRouteStartForm'",
  top: 100000,
  limit: 100000
});

console.log("forms count", q.estimated);

var forms = get_individuals(veda.ticket, q.result);

var parts_uris = [];
var contrs_uris = [];

forms.map(function (form) {
  var part_uri = form["s-wf:SimpleRouteStartForm_participant"] && form["s-wf:SimpleRouteStartForm_participant"][0].data;
  var contr_uri = form["s-wf:SimpleRouteStartForm_controller"] && form["s-wf:SimpleRouteStartForm_controller"][0].data;
  if (part_uri) parts_uris.push(part_uri);
  if (contr_uri) contrs_uris.push(part_uri);
});

var parts = get_individuals(veda.ticket, parts_uris);
var contrs = get_individuals(veda.ticket, contrs_uris);

var deleted_parts = [];
var deleted_contrs = [];

parts.map(function (part) {
  if (part["v-s:deleted"] && part["v-s:deleted"][0].data === true) {
    deleted_parts.push(part["@"]);
  }
});
contrs.map(function (contr) {
  if (contr["v-s:deleted"] && contr["v-s:deleted"][0].data === true) {
    deleted_contrs.push(contr["@"]);
  }
});

var deleted_parts_unique = unique(deleted_parts);
var deleted_contrs_unique = unique(deleted_contrs);

console.log("deleted_parts", deleted_parts.length, JSON.stringify(deleted_parts));
console.log("deleted_parts_unique", deleted_parts_unique.length, JSON.stringify(deleted_parts_unique));

console.log("deleted_contrs", deleted_contrs.length, JSON.stringify(deleted_contrs));
console.log("deleted_contrs_unique", deleted_contrs_unique.length, JSON.stringify(deleted_contrs_unique));

function unique(arr) {
  var n = {}, r = [];
  for(var i = 0; i < arr.length; i++) {
    if (!n[arr[i]]) {
      n[arr[i]] = true;
      r.push(arr[i]);
    }
  }
  return r;
}



//tasks fixes

var q = query({
  ticket: veda.ticket,
  query: "'rdf:type'==='v-wf:DecisionForm' && 'v-wf:isCompleted' == 'false'",
  top: 100000,
  limit: 100000
});

console.log("tasks count", q.estimated);

var tasks = get_individuals(veda.ticket, q.result);

var receivers_uris = [];

tasks.map(function (task) {
  if (task["v-wf:to"]) {
    receivers_uris.push(task["v-wf:to"][0].data);
  }
});

var receivers = get_individuals(veda.ticket, receivers_uris);

var deleted = [];

receivers.map(function (receiver) {
  if (receiver["v-s:deleted"] && receiver["v-s:deleted"][0].data === true) {
    deleted.push(receiver["@"]);
  }
});

var deleted_unique = unique(deleted);

console.log("deleted", deleted.length, JSON.stringify(deleted));
console.log("deleted_unique", deleted_unique.length, JSON.stringify(deleted_unique));
