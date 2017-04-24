//#360 Назначения <-> Задачи
// 3. Всем пользователям назначение по-умолчанию из SAP HR.

var apps_uris = query({
    ticket: veda.ticket,
    query: "'rdf:type'==='v-s:Appointment' && 'v-s:official' == true && 'v-s:origin'=='+SAP +HR'",
    top: 100000,
    limit: 100000
  }).result;

var apps = get_individuals(veda.ticket, apps_uris);
var update_count = 0;
var undefined_count = 0;
var total = apps.length;
apps.forEach(function (app, i) {
  var app_uri = app["@"];
  var pers_uri = app["v-s:employee"] && app["v-s:employee"][0] && app["v-s:employee"][0].data;
  if (pers_uri) {
    var pers = get_individual(veda.ticket, pers_uri);
    if (
      !(pers["v-s:defaultAppointment"] && pers["v-s:defaultAppointment"][0] && pers["v-s:defaultAppointment"][0].data)
      || pers["v-s:defaultAppointment"][0].data !== app_uri
    ) {
      var old = pers["v-s:defaultAppointment"] && pers["v-s:defaultAppointment"][0] && pers["v-s:defaultAppointment"][0].data;
      if (!old) ++undefined_count;
      ++update_count;
      pers["v-s:defaultAppointment"] = [{data: app_uri, type: "Uri"}];
      //put_individual(veda.ticket, pers);
      if ( (i+1) % 50 === 0 || (i+1) === total) console.log(i+1, "of", total, "update_count", update_count, "undefined_count", undefined_count);
    }
  }
});
