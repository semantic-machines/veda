// Fix mnd-s:AccountingDoc

var file_uris = [];
veda.Util.processQuery("'rdf:type'==='v-s:File' && 'v-s:created'==[2017-03-31T21:00:00.000Z,2017-08-09T21:00:00.000Z]", 1000000, 10000, 1000, function (uri) {
  file_uris.push(uri);
});


file_uris.forEach(function (file_uri, index, arr) {
  try {

    if (index % 10000 === 0) {
      console.log("%d done of %d", index + 1, arr.length);
    }

    var file = get_individual(veda.ticket, file_uri);

    if (!file) {
      console.log("no file: %s", file_uri);
      return;
    }

    var is_file = file["rdf:type"] && file["rdf:type"][0] && file["rdf:type"][0].data && file["rdf:type"][0].data === "v-s:File";

    if (!is_file) return;

    var file_deleted = file["v-s:deleted"] && file["v-s:deleted"][0] && file["v-s:deleted"][0].data && file["v-s:deleted"][0].data === true;

    if (file_deleted) return;

    var parent_uri = file["v-s:parent"] && file["v-s:parent"][0] && file["v-s:parent"][0].data;

    if (!parent_uri) {
      console.log("no parent in file: %s", file_uri);
      return;
    }

    var parent = get_individual(veda.ticket, parent_uri);

    if (!parent) {
      console.log("no parent: %s", parent_uri);
      return;
    }

    var is_accounting_doc = parent["rdf:type"] && parent["rdf:type"][0] && parent["rdf:type"][0].data && parent["rdf:type"][0].data === "mnd-s:AccountingDoc";

    if (!is_accounting_doc) return;

    var has_attachment = parent["v-s:attachment"] && parent["v-s:attachment"].length > 0;

    if (!has_attachment) {
      parent["v-s:attachment"] = [{
        "data": file_uri,
        "type": "Uri"
      }];

      put_individual(veda.ticket, parent);
      console.log("(1) fixed: %s", parent["@"]);
      return ;
    }

    var has_this_file = parent["v-s:attachment"].filter(function (value) {
      return value.data === file_uri;
    }).length;

    if (has_this_file) return;

    parent["v-s:attachment"].push({
      "data": file_uri,
      "type": "Uri"
    });

    put_individual(veda.ticket, parent);
    console.log("(2) fixed: %s", parent["@"]);

  } catch (err) {
    console.log(err);
  }
});



