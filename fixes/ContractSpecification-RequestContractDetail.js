//было - mnd-s:hasContractSpecification
//стало - mnd-s:hasRequestContractDetail

//стало - mnd-s:RequestContractDetail
//было - mnd-s:ContractSpecification

var contracts_uris = query({
  ticket: veda.ticket,
  query: "( 'mnd-s:hasContractSpecification.isExists' == true )",
  top: 1000,
  limit: 1000
}).result;

var contracts = get_individuals(veda.ticket, contracts_uris);

var errors = 0;

var processed = 0;

contracts.map( function (contract) {

  ++processed;

  contract["mnd-s:hasRequestContractDetail"] = contract["mnd-s:hasContractSpecification"];
  delete contract["mnd-s:hasContractSpecification"];

  var detail_uri = contract["mnd-s:hasRequestContractDetail"][0].data;
  var detail;

  try {

    detail = get_individual(veda.ticket, detail_uri);

  } catch (error) {

    console.log(++errors, "no spec", contract["@"]);
    delete contract["mnd-s:hasRequestContractDetail"];

    put_individual(veda.ticket, contract);

    return contract["@"] + "_NO_SPEC";
  }

  detail["rdf:type"] = [{type:"Uri", data:"mnd-s:RequestContractDetail"}];

  put_individual(veda.ticket, detail);

  put_individual(veda.ticket, contract);

  return contract["@"] + "_" + detail["@"];
});



/*

Заявка ИТ  57990

Нужен скрипт, который исправить все миллион контрактов и допов в OF

1) Нужно взять документ если у него не заполнено поле v-s:hasRegistrationRecord
2) Нужно создать индивид класса mnd-s:ContractRegistrationRecord и положить в это поле.
3) Автора и дату создания взять по документу Вложение, которое лижит в поле  v-s:scanAttachment.
4) Взять значение из поля v-s:scanAttachment и поместить в поле  v-s:attachment в созданную рег запись.
5) И еще перенести из исходного документа поле mnd-s:hasOriginalSource в аналогичное
6) И потом удалить поля mnd-s:hasOriginalSource   и  v-s:scanAttachment  из обрабатываемого контракта, допа

*/

var contracts_uris = query({
  ticket: veda.ticket,
  query: "('rdf:type'==='mnd-s:Contract' || 'rdf:type'==='mnd-s:AdditionalAgreement') && 'v-s:scanAttachment.isExists'== true ",
  top: 100000,
  limit: 100000,
}).result;

console.log("total to process", contracts_uris.length);

var no_contract = 0,
    has_record = 0,
    no_scan = 0,
    changed = 0;

processPortion();

function processPortion () {
  var portion = contracts_uris.splice(-100);
  portion.forEach( processContract );
  if (contracts_uris.length) {
    console.log("left to process", contracts_uris.length);
    setTimeout(processPortion, 10000);
  } else {
    console.log("all done", contracts_uris.length);
  }
  console.log (
    "no_contract", no_contract,
    "has_record", has_record,
    "no_scan", no_scan,
    "changed", changed
  );
}

function processContract (contract_uri) {

  var contract = get_individual(veda.ticket, contract_uri);
  if (!contract) {
    ++no_contract;
    return;
  }

  // 1)
  if (contract["v-s:hasRegistrationRecord"] && contract["v-s:hasRegistrationRecord"].length) {
    ++has_record;
    return;
  }

  if (!contract["v-s:scanAttachment"] || !contract["v-s:scanAttachment"].length) {
    ++no_scan;
    return;
  }

  var scan0_uri = contract["v-s:scanAttachment"][0].data;

  var scan0 = get_individual(veda.ticket, scan0_uri);

  // 2)
  var record = {
    "@": veda.Util.genUri(),
    "rdf:type": [{type: "Uri", data: "mnd-s:ContractRegistrationRecord"}],
    // 4)
    "v-s:attachment": contract["v-s:scanAttachment"],
    // 3)
    "v-s:creator": scan0["v-s:creator"],
    "v-s:created": scan0["v-s:created"],
    // 5)
    "mnd-s:hasOriginalSource": contract["mnd-s:hasOriginalSource"],
    "v-s:backwardProperty": [{type: "Uri", data: "v-s:hasRegistrationRecord"}],
    "v-s:backwardTarget": [{type: "Uri", data: contract["@"]}]
  };

  // 2)
  contract["v-s:hasRegistrationRecord"] = [{type: "Uri", data: record["@"]}];

  // 6)
  delete contract["v-s:scanAttachment"];
  delete contract["mnd-s:hasOriginalSource"];

  put_individual(veda.ticket, contract);
  put_individual(veda.ticket, record);
  //console.log("put contract", JSON.stringify(contract));
  //console.log("put record", JSON.stringify(record));
  ++changed;
}
