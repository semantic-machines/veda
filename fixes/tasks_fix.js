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





/* Переиндексировать задачи и перевыдать задачи на персону и должность
 * Выбрать все задачи
 * Для каждой задачи:
 *  1) Добавить в полях v-wf:from, v-wf:to персон соответсвующих должности (взять из v-wf:WorkItem)
 *  2) Выдать права на чтение найденным персонам
 *  3) Вписать дату создания задачи
 *  4) Вписать автора задачи = системная учетка
 */

var tasks_uris = query({
  ticket: veda.ticket,
  query: "'rdf:type'==='v-wf:DecisionForm'",
  top: 1000000,
  limit: 1000000
}).result;
console.log("total to process", tasks_uris.length);

var err = 0;

process(tasks_uris, 100, 10000, processTask);

function processTask(task_uri) {
  var task = get_individual(veda.ticket, task_uri);
  task["v-s:creator"] = [{type: "Uri", data: "cfg:VedaSystem"}];

  var doc_uri = task["v-wf:onDocument"] && task["v-wf:onDocument"][0].data;
  if (!doc_uri) { console.log(++err, "no doc_uri found for task", task["@"]); return; }
  var doc = get_individual(veda.ticket, doc_uri);
  if (!doc) { console.log(++err, "no doc found for task", task["@"]); return; }
  task["v-s:created"] = doc["v-s:created"];

  var work_order_uri = task["v-wf:onWorkOrder"] && task["v-wf:onWorkOrder"][0].data;
  if (!work_order_uri) { console.log(++err, "no work_order_uri found for task", task["@"]); return; }
  var work_order = get_individual(veda.ticket, work_order_uri);
  if (!work_order) { console.log(++err, "no work_order found for task", task["@"]); return; }

  var work_item_uri = work_order["v-wf:forWorkItem"] && work_order["v-wf:forWorkItem"][0].data;
  if (!work_item_uri) { console.log(++err, "no work_item_uri found for task", task["@"]); return; }
  var work_item = get_individual(veda.ticket, work_item_uri);
  if (!work_item) { console.log(++err, "no work_item found for task", task["@"]); return; }

  var process_uri = work_item["v-wf:forProcess"] && work_item["v-wf:forProcess"][0].data;
  if (!process_uri) { console.log(++err, "no process_uri found for task", task["@"]); return; }
  var process = get_individual(veda.ticket, process_uri);
  if (!process) { console.log(++err, "no process found for task", task["@"]); return; }

  var local_vars = process["v-wf:localVars"];
  if (!local_vars) { console.log(++err, "no local_vars found for task", task["@"]); return; }

  var initiator;
  var actor;

  for (var i = 0, variable_obj; (variable_obj = local_vars[i]); i++) {
    var variable_uri = variable_obj.data;
    var variable = get_individual(veda.ticket, variable_uri);
    var name = variable["v-wf:variableName"] && variable["v-wf:variableName"][0].data;
    if ( name === "initiator" ) {
      initiator = variable["v-wf:variableValue"] && variable["v-wf:variableValue"][0].data;
    }
    if ( name === "actor" ) {
      actor = variable["v-wf:variableValue"] && variable["v-wf:variableValue"][0].data;
    }
  }
  if (!initiator) { console.log(++err, "no initiator found for task", task["@"]); return; }
  if (!actor) { console.log(++err, "no actor found for task", task["@"]); return; }

  var initiator_person = initiator["v-s:employee"] && initiator["v-s:employee"][0];
  var initiator_position = initiator["v-s:occupation"] && initiator["v-s:occupation"][0];
  if (!initiator_person || !initiator_position) { console.log(++err, "no initiator_person || initiator_position found for task", task["@"]); return; }
  console.log( "right for initiator person =", initiator_person.data );
  //addRight(veda.ticket, [can_read], initiator_person.data, task_uri);

  var actor_person = actor["v-s:employee"] && actor["v-s:employee"][0];
  var actor_position = actor["v-s:occupation"] && actor["v-s:occupation"][0];
  if (!actor_person || !actor_position) { console.log(++err, "no actor_person || actor_position found for task", task["@"]); return; }
  console.log( "right for actor person =", actor_person.data );
  //addRight(veda.ticket, [can_read], actor_person.data, task_uri);

  task["v-wf:from"] = [initiator_person, initiator_position];
  task["v-wf:to"] = [actor_person, actor_position];

  console.log( "task update =", JSON.stringify(task) );
  //put_individual(veda.ticket, task);
}

function process(uris, delta, pause, fn) {
  var portion = uris.splice(-delta);
  portion.forEach( fn );
  if (uris.length) {
    console.log("left to process", uris.length);
    setTimeout(process, pause, uris, delta, pause, fn);
  } else {
    console.log("all done", uris.length);
  }
}


// re-index tasks
var tasks_uris = query({
  ticket: veda.ticket,
  query: "'rdf:type' === 'v-wf:DecisionForm'",
  top: 100000,
  limit: 100000
}).result;

process(tasks_uris, 100, 5000, reindexTask);

function reindexTask (task_uri) {
  var task = get_individual(veda.ticket, task_uri);
  //console.log("task_uri", task_uri);
  put_individual(veda.ticket, task);
}

function process(uris, delta, pause, fn) {
  console.log("left to process", uris.length);
  var portion = uris.splice(-delta);
  portion.forEach( fn );
  if (uris.length) {
    setTimeout(process, pause, uris, delta, pause, fn);
  } else {
    console.log("all done", uris.length);
  }
}

// re-index tasks
var tasks_uris = query({
  ticket: veda.ticket,
  query: "'rdf:type' === 'v-wf:DecisionForm'",
  top: 100000,
  limit: 100000
}).result;

processResult(tasks_uris, 100, 5000, reindexTask);

function reindexTask (task_uri) {
  var task = get_individual(veda.ticket, task_uri);
  //console.log("task_uri", task_uri);
  put_individual(veda.ticket, task);
}

function processResult(uris, delta, pause, fn) {
  console.log("left to process", uris.length);
  var portion = uris.splice(-delta);
  portion.forEach( fn );
  if (uris.length) {
    setTimeout(process, pause, uris, delta, pause, fn);
  } else {
    console.log("all done", uris.length);
  }
}


// DONE: re-index tasks
veda.Util.processQuery("'rdf:type'==='v-wf:DecisionForm'", 100000, 100, 10000, function (task_uri) {
  var task = get_individual(veda.ticket, task_uri);
  put_individual(veda.ticket, task);
});


// DONE: fix 'v-wf:to.rdf:type'==='v-s:Appointment' -> v-wf:to = person + position
veda.Util.processQuery("'rdf:type'==='v-wf:DecisionForm' && 'v-wf:to.rdf:type'==='v-s:Appointment'", 100000, 100, 10000, function (task_uri) {
  var task = get_individual(veda.ticket, task_uri);
  var app_uri = task["v-wf:to"] && task["v-wf:to"][0].data;
  var app = get_individual(veda.ticket, app_uri);
  var emp = app["v-s:employee"];
  var pos = app["v-s:occupation"];
  if (emp && pos) {
    task["v-wf:to"] = [].concat(emp, pos);
    //console.log("fixed task:", JSON.stringify(task));
    put_individual(veda.ticket, task);
  } else {
    console.log("error in task:", task_uri);
  }
});


// DONE: fix 'v-wf:from.rdf:type'==='v-s:Appointment' -> v-wf:from = person + position
veda.Util.processQuery("'rdf:type'==='v-wf:DecisionForm' && 'v-wf:from.rdf:type'==='v-s:Appointment'", 100000, 100, 15000, function (task_uri) {
  var task = get_individual(veda.ticket, task_uri);
  var app_uri = task["v-wf:from"] && task["v-wf:from"][0].data;
  var app = get_individual(veda.ticket, app_uri);
  var emp = app["v-s:employee"];
  var pos = app["v-s:occupation"];
  if (emp && pos) {
    task["v-wf:from"] = [].concat(emp, pos);
    //console.log("fixed task:", JSON.stringify(task));
    put_individual(veda.ticket, task);
  } else {
    console.log("error in task:", task_uri);
  }
});


// DONE: fix 'v-wf:to.rdf:type'==='v-s:Position' -> v-wf:to = person + position
veda.Util.processQuery("'rdf:type'==='v-wf:DecisionForm' && ('v-wf:to.rdf:type'==='v-s:Position' && 'v-wf:to.rdf:type'!='v-s:Person')", 1000, 100, 0, function (task_uri, index) {
  try {

    var task = get_individual(veda.ticket, task_uri);

    var work_order_uri = getUri(task["v-wf:onWorkOrder"]);
    var work_order = get_individual(veda.ticket, work_order_uri);

    var work_item_uri = getUri(work_order["v-wf:forWorkItem"]);
    var work_item = get_individual(veda.ticket, work_item_uri);

    var process_uri = getUri(work_item["v-wf:forProcess"]);
    var process = get_individual(veda.ticket, process_uri);

    var executor_uri = getUri(process["v-wf:executor"]);
    var executor = get_individual(veda.ticket, executor_uri);

    var emp = executor["v-s:employee"];
    var pos = executor["v-s:occupation"];

    if (emp && pos) {
      task["v-wf:to"] = [].concat(emp, pos);
      console.log("fixed task:", task_uri, "v-wf:to", JSON.stringify(task["v-wf:to"]));
      //put_individual(veda.ticket, task);
    }

  } catch (err) {
    console.log("Error.", "| task_uri", task_uri, "| work_order_uri:", work_order_uri, "| work_item_uri", work_item_uri, "| process_uri", process_uri, "| executor_uri", executor_uri);
  }
});
