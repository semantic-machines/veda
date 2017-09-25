// Передернуть контракты

var types = [
  "v-s:Contract",
  "v-s:ContractPassport"
];

types.map(function (type_uri) {

  console.log("type:",  type_uri);

  var individuals = query({
    ticket: veda.ticket,
    query: "'rdf:type'=='" + type_uri + "'",
    top: 1000000,
    limit: 1000000
  }).result;

  console.log("individual count =",  individuals.length);

  individuals.map(function (individual_uri) {
    var individual = get_individual(veda.ticket, individual_uri);
    put_individual(veda.ticket, individual);
  });

});


// Удалить назначения на должность "уволенный сотрудник"

var dismissed = query({
  ticket: veda.ticket,
  query: "'rdf:type'==='v-s:Appointment' && 'v-s:occupation'=='d:mondi_position_dismissed'",
  limit: 1000000,
  top: 1000000
}).result;

console.log("dismissed =", dismissed.length);

dismissed.map(function (appointment_uri, index) {
  var appointment = get_individual(veda.ticket, appointment_uri);
  appointment["v-s:deleted"] = [{
    data: true,
    type: "Boolean"
  }];
  put_individual(veda.ticket, appointment);
  //console.log(appointment);
});


// Удалить персоны которые были на должности "уволенный сотрудник"

var dismissed = query({
  ticket: veda.ticket,
  query: "'rdf:type'==='v-s:Appointment' && 'v-s:occupation'=='d:mondi_position_dismissed' && 'v-s:deleted'==true",
  limit: 1000000,
  top: 1000000
}).result;

console.log("deleted dismissed =", dismissed.length);
var count = 0;
dismissed.map(function (appointment_uri, index) {
  var appointment = get_individual(veda.ticket, appointment_uri);
  var person_uri = appointment["v-s:employee"] && appointment["v-s:employee"][0].data;
  if (!person_uri) return;

  var person = get_individual(veda.ticket, person_uri);
  person["v-s:deleted"] = [{
    data: true,
    type: "Boolean"
  }];
  count++;
  put_individual(veda.ticket, person);
  //console.log(appointment);
});
console.log("persons deleted count =", count);
