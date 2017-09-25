// AUDIT INTERNAL FIX
// planned = 662b7b2a611446f88fa6f7353bdf47ca (true)
// unplanned = b4519ba4667b4f26b8f8a9ec03e07a78 (false)

var planned = 0, unplanned = 0, undef = 0;

veda.Util.processQuery("'rdf:type'==='mnd-s:AuditInternal'", 10000, 100, 100, function (uri) {
  try {
    var audit = new veda.IndividualModel(uri);
    if (
      audit.hasValue("mnd-s:auditInternalUnplanned", "d:662b7b2a611446f88fa6f7353bdf47ca")
      || audit.hasValue("mnd-s:auditInternalUnplanned", true)
    ) {
      audit.set("mnd-s:auditInternalPlanned", [true]);
      console.log(++planned, "audit = %s, planned = true", uri);
    } else if (
      audit.hasValue("mnd-s:auditInternalUnplanned", "d:b4519ba4667b4f26b8f8a9ec03e07a78")
      || audit.hasValue("mnd-s:auditInternalUnplanned", false)
    ) {
      audit.set("mnd-s:auditInternalPlanned", [false]);
      console.log(++unplanned, "audit = %s, planned = false", uri);
    } else {
      audit.set("mnd-s:auditInternalPlanned", []);
      audit.set("mnd-s:auditInternalUnplanned", []);
      console.log(++undef, "audit = %s, planned = []", uri);
    }
    //audit.save();
  } catch (err) {
    console.log(err);
  }
});
