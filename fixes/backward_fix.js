// Fix backward links

var err_notarget_count = 0;
var err_bl_count = 0;

veda.Util.processQuery("'v-s:backwardTarget.isExists'==true ", 1000, 1000, 100, function (link_uri) {
  try {
    var link = new veda.IndividualModel(link_uri),
        target = link.hasValue("v-s:backwardTarget") && link["v-s:backwardTarget"][0],
        property_uri = link.hasValue("v-s:backwardProperty") && link["v-s:backwardProperty"][0].id;

    if (target && property_uri) {

      if ( !target.hasValue(property_uri, link) ) {
        console.log(++err_bl_count, "error bl", "| link_uri", link.id,  "| target_uri", target.id, "| property_uri", property_uri);
        /*link.isSync(false);
        link.save();*/
      }

    } else {
      console.log(++err_notarget_count, "no target OR no property",  "| link_uri:", link.id, "| target_uri:", target.id, "| property_uri", property_uri);
    }

  } catch (err) {
    console.log(err);
  }
});


// Fix mnd-s:ContractorCategoryRequest

veda.Util.processQuery("'rdf:type'==='mnd-s:ContractorCategoryRequest'", 10000, 100, 100, function (uri) {
  try {
    var obj = get_individual(veda.ticket, uri);
    put_individual(veda.ticket, obj);
  } catch (err) {
    console.log(err);
  }
});

// Fix mnd-s:ContractorCategoryDecision

veda.Util.processQuery("'rdf:type'==='mnd-s:ContractorCategoryDecision'", 10000, 100, 100, function (uri) {
  try {
    var obj = get_individual(veda.ticket, uri);
    put_individual(veda.ticket, obj);
  } catch (err) {
    console.log(err);
  }
});


// Fix v-s:ContractorProfile

veda.Util.processQuery("'rdf:type'==='v-s:ContractorProfile'", 20000, 100, 30000, function (uri) {
  try {
    var obj = get_individual(veda.ticket, uri);
    put_individual(veda.ticket, obj, false);
  } catch (err) {
    console.log(err);
  }
});

