// Восстанавливаем затертые входящие письма с электронной почты

function get_individual_59 (uri) {
  //console.log("get", uri);
  return $.get({
    url: "http://syk-vs59.mp.local:8080/get_individual",
    data: {
      ticket: "6c66d498-ed90-4ea5-b209-88a463f5c424",
      uri: uri
    },
    dataType: "json"
  });
}

function get_individuals_59 (uris) {
  //console.log("get_all", uris);
  return $.post({
    url: "http://syk-vs59.mp.local:8080/get_individuals",
    data: JSON.stringify({
      "ticket": "6c66d498-ed90-4ea5-b209-88a463f5c424",
      "uris": uris
    }),
    contentType: "application/json"
  });
}

veda.Util.processQuery("( ( 'rdf:type'==='mnd-s:IncomingLetter' ) && ( 'v-s:description'=='+Получено* +с* +почтового* +ящика* +Syktyvkar.DocFlowODO@mondigroup.com*' ) && ( 'v-s:created'==[2017-05-30T21:00:00.000Z,2017-06-29T20:59:59.999Z] ) )", 10000, 100, 0, function (letter_uri) {
//veda.Util.processQuery("( '@'=='d:b70be799c85049349ba64c61eb18b1de' && ( 'rdf:type'==='mnd-s:IncomingLetter' ) && ( 'v-s:description'=='+Получено* +с* +почтового* +ящика* +Syktyvkar.DocFlowODO@mondigroup.com*' ) && ( 'v-s:created'==[2017-05-30T21:00:00.000Z,2017-06-29T20:59:59.999Z] ) )", 10000, 100, 0, function (letter_uri) {
  try {
    get_individual_59(letter_uri).then(function (letter) {
      var letterStr = JSON.stringify(letter);
      //console.log("letterStr", letterStr);
      var re = new RegExp(letter_uri + "_\\d{1}", "g");
      var sub_uris = [];
      letterStr.replace(re, function(sub_uri) {
        //console.log("sub_match", sub_uri);
        sub_uris.push(sub_uri);
      });
      get_individuals_59(sub_uris).then(function (subs) {
        subs.map(function (sub) {
          put_individual(veda.ticket, sub);
          console.log("sub:", sub["@"]);
        });
        put_individual(veda.ticket, letter);
        console.log("letter:", letter["@"]);
      });
    });
  } catch (err) {
    console.log(err, "Error", "| letter_uri", letter_uri);
  }
});

