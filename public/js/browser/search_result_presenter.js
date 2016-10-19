// Search Result Presenter

veda.Module(function SearchResultPresenter(veda) { "use strict";

  function renderIndividualProperty (veda, individual, property_uri, template, container) {
    var label, uri, values;
    var property = new veda.IndividualModel(property_uri);
    label = property["rdfs:label"].join(", ");
    uri = property.id;
    values = individual[property_uri]
          .map( function (item) {
            if (item instanceof String)
              // Check if string starts with http:// or ftp://
              return item.search(/^.{3,5}:\/\//) === 0 ? "<a target='_blank' href='" + item + "'>" + item + "</a>" : item ;
            else if (item instanceof veda.IndividualModel)
              return "<a href='#/" + item.id + "'>" +
                (item["rdfs:label"] && item["rdfs:label"].length ? item["rdfs:label"].join(", ") : item.id) + "</a>";
            else return item;
          })
          .join(", ");
    container.append (
      riot.render (
        template,
        {
          label: label,
          uri: uri,
          values: values
        }
      )
    );
  }

  // Get templates
  var displayedPropertiesLimit = 10;
  var individual_template = $("#individual-template").html();
  var individual_single_property_template = $("#individual-single-property-template").html();
  var individual_label_template = $("#individual-label-template").html();

  //var cnt = 0;

  veda.on("search_result:loaded", function (individual, container) {

    //console.log("search_result presenter:", ++cnt, individual.id);

    if (!container) return;

    container.append(individual_template);

    // Render individual title
    $("#individual-label", container).append(
      riot.render(
        individual_label_template,
        {
          label: individual["rdfs:label"] && individual["rdfs:label"].length ? individual["rdfs:label"].join(", ") : individual.id,
          uri: individual.id
        }
      )
    );

    // Render individual properties
    Object.getOwnPropertyNames(individual.properties).reduce ( function (limit, property_uri) {
      if(property_uri === "@") { return }
      if (limit <= 0) return limit;
      try {
        renderIndividualProperty (veda, individual, property_uri, individual_single_property_template, $("#individual-properties", container));
      } catch (e) {}
      return --limit;
    }, displayedPropertiesLimit);

  });

});
