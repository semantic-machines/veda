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
