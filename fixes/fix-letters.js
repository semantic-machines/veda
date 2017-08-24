// Re-index letters
veda.Util.processQuery("'rdf:type'==='mnd-s:OutgoingLetter' || 'rdf:type'==='mnd-s:IncomingLetter'", 1000, 10, 100, function (letter_uri) {
  try {
    var letter = get_individual(veda.ticket, letter_uri);
    put_individual(veda.ticket, letter);
  } catch (err) {
    console.log(err);
  }
});


'rdf:type'==='mnd-s:IncomingLetter' && 'v-s:hasLetterRegistrationRecordRecipient.isExists' == true && 'v-s:hasLetterRegistrationRecordRecipient.rdf:type' != 'v-s:LetterRegistrationRecordRecipient'
778 штук

'rdf:type'==='mnd-s:IncomingLetter' && 'v-s:hasLetterRegistrationRecordSender.isExists' == true && 'v-s:hasLetterRegistrationRecordSender.rdf:type' != 'v-s:LetterRegistrationRecordSender'
1 штука

'rdf:type'==='mnd-s:OutgoingLetter' && 'v-s:hasLetterRegistrationRecordRecipient.isExists' == true && 'v-s:hasLetterRegistrationRecordRecipient.rdf:type' != 'v-s:LetterRegistrationRecordRecipient'
0 штук

'rdf:type'==='mnd-s:OutgoingLetter' && 'v-s:hasLetterRegistrationRecordSender.isExists' == true && 'v-s:hasLetterRegistrationRecordSender.rdf:type' != 'v-s:LetterRegistrationRecordSender'
7 штук


// rdfs:Resource && v-s:created exists
var cnt = 0;
veda.Util.processQuery("'rdf:type'==='rdfs:Resource' && 'v-s:created.isExists'==true ", 1000, 10, 100, function (uri) {
  try {
    var resource = new veda.IndividualModel(uri);
    var created = resource["v-s:created"][0].toISOString();
    console.log("%d uri = %s, created = %s", ++cnt, uri, created);
  } catch (err) {
    console.log("uri = %s", uri, err);
  }
});

// rdfs:Resource -> mnd-s:Correspondent
var cnt = 0, err = 0;
veda.Util.processQuery("'rdf:type'==='rdfs:Resource' && 'v-s:correspondentOrganization.isExists'==true ", 1000, 10, 100, function (uri) {
  try {
    var correspondent = new veda.IndividualModel(uri);
    correspondent["rdf:type"] = [ new veda.IndividualModel("mnd-s:Correspondent") ];
    //correspondent.save();
    console.log("%d done, %s", ++cnt, uri);
  } catch (error) {
    console.log("%d error, %s", ++err, uri, error);
  }
});
