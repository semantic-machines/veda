"use strict";

// Veda browser utility functions

veda.Module(function (veda) { "use strict";

  veda.Util = veda.Util || {};

  // Уcтанавливает cookie
  // name - Название cookie
  // value - Значение cookie (строка)
  // props - Объект с дополнительными свойствами для установки cookie:
  //   expires - Время истечения cookie. Интерпретируется по-разному, в зависимости от типа:
  //     Если число - количество миллисекунд до истечения.
  //     Если объект типа Date - точная дата истечения.
  //     Если expires в прошлом, то cookie будет удалено.
  //     Если expires отсутствует или равно 0, то cookie будет установлено как сессионное и исчезнет при закрытии браузера.
  //   path - Путь для cookie.
  //   domain - Домен для cookie.
  //   secure - Пересылать cookie только по защищенному соединению.

  veda.Util.setCookie = function (name, value, props) {
    props = props || {};
    var exp = props.expires;
    if (typeof exp === "number" && exp) {
      var d = new Date();
      d.setTime(d.getTime() + exp);
      exp = props.expires = d;
    }
    if (exp && exp.toUTCString) { props.expires = exp.toUTCString(); }
    value = encodeURIComponent(value);
    var updatedCookie = name + "=" + value;
    for(var propName in props) {
      updatedCookie += "; " + propName;
      var propValue = props[propName];
      if (propValue !== true) { updatedCookie += "=" + propValue; }
    }
    document.cookie = updatedCookie;
  };

  veda.Util.delCookie = function (name) {
    veda.Util.setCookie(name, null, { expires: -1 });
  };

  // Escape function for css (jQuery) selectors
  veda.Util.escape4$ = function (str) {
    if (str) return str.replace(/([ #;?%&,.+*~\':"!^$[\]()=>|\/@])/g,'\\$1');
    return str;
  };

  veda.Util.toTTL = function (individualList, callback) {
    var ontologies = query(veda.ticket, "'rdf:type'=='owl:Ontology'").result,
        all_prefixes = {},
        prefixes = {},
        triples = [],
        writer = N3.Writer();

    all_prefixes["dc"] = "http://purl.org/dc/elements/1.1/";
    all_prefixes["grddl"] = "http://www.w3.org/2003/g/data-view#";
    ontologies.map( function (ontology_uri) {
      var ontology = new veda.IndividualModel(ontology_uri);
      var prefix = ontology_uri.slice(0, -1);
      if (ontology.hasValue("v-s:fullUrl")) {
        all_prefixes[prefix] = ontology["v-s:fullUrl"][0].toString();
      }
    });

    function prefixer(uri) {
      try {
        var colonIndex = uri.indexOf(":"),
            prefix = uri.substring(0, colonIndex);
        if ( !prefixes[prefix] ) {
          prefixes[prefix] = all_prefixes[prefix];
        }
        if ( colonIndex === uri.length-1 ) {
          return prefixes[prefix];
        } else {
          return N3.Util.expandPrefixedName(uri, prefixes);
        }
      } catch (error) {
        var notify = veda.Notify ? new veda.Notify() : function () {};
        notify("danger", error);
        return uri;
      }
    }

    individualList.each(function (individual) {
      var subject = prefixer(individual.id);
      // Type first
      individual.properties["rdf:type"].map(function (value) {
        var type_triple = {};
        type_triple.subject = subject;
        type_triple.predicate = prefixer("rdf:type");
        type_triple.object = prefixer(value.data);
        triples.push(type_triple);
      });
      // Other properties
      Object.getOwnPropertyNames(individual.properties).sort().map(function (property_uri) {
        if (property_uri === "@" || property_uri === "rdf:type") { return; }
        individual.properties[property_uri].map(function (item) {
          var triple = {};
          triple.subject = subject;
          triple.predicate = prefixer(property_uri);
          var value = item.data,
              type = item.type,
              lang = item.lang;
          switch (type) {
            case 4:
            case "Integer":
              triple.object = '"' + value + '"^^' + prefixer("xsd:integer");
              break;
            case 32:
            case "Decimal":
              triple.object = '"' + value + '"^^' + prefixer("xsd:decimal");
              break;
            case 64:
            case "Boolean":
              triple.object = '"' + value + '"^^' + prefixer("xsd:boolean");
              break;
            case 2:
            case "String":
              triple.object = lang && lang !== "NONE" ? '"' + value + '"@' + lang.toLowerCase() : '"' + value + '"^^' + prefixer("xsd:string");
              break;
            case 8:
            case "Datetime":
              triple.object = '"' + ( value instanceof Date ? value.toISOString() : value ) + '"^^' + prefixer("xsd:dateTime");
              break;
            case 1:
            case "Uri":
              triple.object = prefixer(value);
              break;
          }
          triples.push(triple);
        });
      });
    });
    writer.addPrefixes(prefixes);
    writer.addTriples(triples);
    writer.end(callback);
  };

  veda.Util.exportTTL = function (individualList) {
    veda.Util.toTTL(individualList, function (error, result) {
      var blob = new Blob([result], {type: "text/plain;charset=utf-8"});
      saveAs(blob, "exported_graph.ttl");
    });
  };

  /**
   * Event `createReport` handler:
   *  - Find available reports or use report specified by `reportId` parameter.
   *  - Let user to choice report (if more then one founded)
   *  - Redirect to report
   */
  veda.Util.createReport = function (individual, reportId) {
    if (reportId !== undefined) {
      $("[resource='" + veda.Util.escape4$(individual.id) + "']").find("#createReport").dropdown("toggle");
      veda.Util.redirectToReport(individual, reportId);
    } else {
      var s = query(veda.ticket, "'rdf:type' == 'v-s:ReportsForClass' && 'v-ui:forClass' == '" + individual["rdf:type"][0].id + "'");
      if (s.result.length === 0) {
        alert('Нет отчета.');
      } else if (s.result.length === 1) {
        $("[resource='" + veda.Util.escape4$(individual.id) + "']").find("#createReport").dropdown("toggle");
        veda.Util.redirectToReport(individual, s.result[0]);
      } else {
        var reportsDropdown = $('[resource="' + veda.Util.escape4$(individual.id) + '"] #chooseReport + .dropdown-menu');
        if (reportsDropdown.html() == "") {
          s.result.forEach( function (res_id) {
            $("<li/>", {
              "style" : "cursor:pointer",
              "html" : "<a href='#'>" + new veda.IndividualModel(res_id)["rdfs:label"].join(" ") + "</a>",
              "click": (function (e) {
                veda.Util.redirectToReport(individual, res_id);
              })
            }).appendTo(reportsDropdown);
          });
        }
      }
    }
  };

  veda.Util.redirectToReport = function (individual, reportId) {
    var jasperServer = new veda.IndividualModel('cfg:jasperServerAddress');
    var jasperServerAddress = jasperServer['rdf:value'][0];
    var report = new veda.IndividualModel(reportId);

    var form = document.createElement("form");
    form.setAttribute("method", "post");
    form.setAttribute("action", jasperServerAddress + "flow.html?_flowId=viewReportFlow&j_username=joeuser&j_password=joeuser&reportUnit=" + encodeURIComponent(report["v-s:reportPath"][0]) + "&output=" + encodeURIComponent(report["v-s:reportFormat"][0]) + "&documentId=" + encodeURIComponent(individual.id) + "&ticket=" + veda.ticket);
    form.setAttribute("target", "view");

    Object.getOwnPropertyNames(individual.properties).forEach(function (key) {
      if ( key !== "@" && individual.hasValue(key) ) {
        var hiddenField = document.createElement("input");
        hiddenField.setAttribute("type", "hidden");
        hiddenField.setAttribute("name", key.replace(":", "_"));
        var value = individual.get(key).map(function (item) {
          return item instanceof veda.IndividualModel ? item.id :
                 item instanceof Date ? item.toISOString() :
                 item;
        }).join(",");
        hiddenField.setAttribute("value", value);
        form.appendChild(hiddenField);
      }
    });
    // Set client timezone parameter
    var tz = (new Date()).getTimezoneOffset();
    var tzField = document.createElement("input");
    tzField.setAttribute("type", "hidden");
    tzField.setAttribute("name", "timezone");
    tzField.setAttribute("value", tz);
    form.appendChild(tzField);
    document.body.appendChild(form);
    window.open("", "view");
    form.submit();
  };

  veda.Util.transform = function (individual, template, transformId) {
    var startForm = veda.Util.buildStartFormByTransformation(individual, new veda.IndividualModel(transformId));
    riot.route("#/" + startForm.id + "///edit");
  };

  /**
   * Event `send` handler:
   *  - Find transformation to start form or use transformation specified by `transformId` parameter
   *  - Apply transformation and redirect to start form.
   */
  veda.Util.send = function (individual, template, transformId, modal, startFormId) {
    if ( transformId ) {
      template.trigger("save");
      var transform = new veda.IndividualModel(transformId);
      var startForm = veda.Util.buildStartFormByTransformation(individual, transform);
      veda.Util.showModal(startForm, startFormId, "edit");
    } else {
      individual["v-wf:hasStatusWorkflow"] = [ new veda.IndividualModel("v-wf:ToBeSent") ];
      template.trigger("save");
      template.closest(".modal").modal("hide").remove();
      var notify = veda.Notify ? new veda.Notify() : function () {};
      notify("success", {name: "Успешно отправлено / Successfully sent"});
    }
  };

  /**
   * Event `showRights` handler:
   *  - Find available reports
   *  - Let user to choice report (if more then one founded)
   *  - Redirect to report
   */
  veda.Util.showRights = function (individual) {
    var modalTmpl = $("#individual-modal-template").html();
    var modal = $(modalTmpl);
    var modalBody = $(".modal-body", modal);
    modal.one("remove", function (e) {
      modal.modal("hide");
    });
    modal.modal();
    $("#main").append(modal);

    var rights = individual['rights'];
    var holder = $("<div>");
    rights.present(holder, "v-ui:PermissionStatementInlineTemplate");
    holder.appendTo(modalBody);

    var origin = individual['rightsOrigin'];
    origin.forEach(function (rightRecord) {
      var holder = $("<div>");
      rightRecord.present(holder, "v-ui:PermissionStatementInlineTemplate");
      holder.appendTo(modalBody);
    });
  };

  veda.Util.showModal = function (individual, template, mode) {
    var modal = $( $("#notification-modal-template").html() );
    modal.modal();
    $("body").append(modal);
    var container = $(".modal-body", modal);
    individual.present(container, template, mode);
    $(".action#cancel", modal).click(function () {
      modal.modal("hide");
    });
    modal.on("hidden.bs.modal", function () {
      modal.remove();
    });
    return modal;
  };

  veda.Util.showSmallModal = function (individual, template, mode) {
    var modal = $( $("#minimal-modal-template").html() );
    modal.modal();
    $("body").append(modal);
    var container = $(".modal-body", modal);
    individual.present(container, template, mode);
    $(".action#cancel", modal).click(function () {
      modal.modal("hide");
    });
    modal.on("hidden.bs.modal", function () {
      modal.remove();
    });
    return modal;
  };
});
