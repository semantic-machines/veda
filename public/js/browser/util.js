// Browser-side utility functions

veda.Module(function Util(veda) { "use strict";

  veda.Util = veda.Util || {};

  veda.Util.clearStorage = function () {
    if ( typeof localStorage !== "undefined" ) {
      delete localStorage["ontology"];
      delete localStorage["user_uri"];
      delete localStorage["end_time"];
      delete localStorage["ticket"];
    }
  }

  // Escape function for css (jQuery) selectors
  veda.Util.escape4$ = function (str) {
    if (str) return str.replace(/([ #;?%&,.+*~\':"!^$[\]()=>|\/@])/g,'\\$1');
    return str;
  };

  veda.Util.guid = function () {
    function s4() {
    return Math.floor((1 + Math.random()) * 0x10000)
           .toString(16)
           .substring(1);
    }
    return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
       s4() + '-' + s4() + s4() + s4();
  };

  veda.Util.genUri = function () {
      return 'd:a' + veda.Util.guid();
  };

  veda.Util.mlstring = function (ru, en) {
      var str_ru = new String(ru);
      str_ru.language = "RU";

      var str_en = new String(en);
      str_en.language = "EN";

      return [str_ru, str_en];
  };

  veda.Util.construct = function (constr, args) {
    function F() {
      return constr.apply(this, args);
    }
    F.prototype = constr.prototype;
    return new F();
  };

  function isInteger(n) { return n % 1 === 0; };

  function zeroPref(n) {
    return n > 9 ? n : "0" + n;
  };

  veda.Util.formatValue = function (value) {
    var formatted;
    switch (true) {
      case value instanceof Date:
        formatted = veda.Util.formatDate(value);
        break;
      case value instanceof Number:
        formatted = veda.Util.formatNumber(value);
        break;
      default:
        formatted = typeof value !== "undefined" ? value.toString() : value;
    }
    return formatted;
  };
  veda.Util.formatDate = function (date) {
    var day = date.getDate(),
      month = date.getMonth() + 1,
      year = date.getFullYear(),
      hours = date.getHours(),
      mins = date.getMinutes(),
      secs = date.getSeconds(),
      fdate, ftime;
    month = zeroPref(month); day = zeroPref(day);
    hours = zeroPref(hours); mins = zeroPref(mins); secs = zeroPref(secs);
    fdate = [day, month, year].join(".");
    ftime = [hours, mins, secs].join(":");
    return (fdate === "01.01.1970" ? "" : fdate) + (ftime === "00:00:00" ? "" : " " + ( secs === "00" ? ftime.substr(0, 5) : ftime) );
  };
  veda.Util.formatNumber = function (n) {
    return (n+"").replace(/.(?=(?:[0-9]{3})+\b)/g, '$& ');
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
        veda.trigger("danger", {status: "TTL:", description: error.message});
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
            case "Integer":
              triple.object = '"' + value + '"^^' + prefixer("xsd:integer");
              break;
            case "Decimal":
              triple.object = '"' + value + '"^^' + prefixer("xsd:decimal");
              break;
            case "Boolean":
              triple.object = '"' + value + '"^^' + prefixer("xsd:boolean");
              break;
            case "String":
              triple.object = lang && lang !== "NONE" ? '"' + value + '"@' + lang.toLowerCase() : '"' + value + '"^^' + prefixer("xsd:string");
              break;
            case "Datetime":
              triple.object = '"' + ( value instanceof Date ? value.toISOString() : value ) + '"^^' + prefixer("xsd:dateTime");
              break;
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

  veda.Util.applyTransform = function (individualList, transform) {
    return transformation(null, individualList, transform, null, null);
  };

  veda.Util.forSubIndividual = function (net, property, id, func) {
    if (net[property]===undefined) return;
    net[property].forEach(function(el) {
      if (el.id == id) {
        func(el);
      }
    });
  };

  veda.Util.removeSubIndividual = function (net, property, id) {
    if (net[property]===undefined) return undefined;
    return net[property].filter( function (item) {
      return item.id !== id;
    });
  };

  /*
   * from http://stackoverflow.com/questions/27266550/how-to-flatten-nested-array-in-javascript
   * by http://stackoverflow.com/users/2389720/aduch
   *
   * This is done in a linear time O(n) without recursion
   * memory complexity is O(1) or O(n) if mutable param is set to false
   */
  veda.Util.flatten = function (array, mutable) {
    var toString = Object.prototype.toString;
    var arrayTypeStr = '[object Array]';

    var result = [];
    var nodes = (mutable && array) || array.slice();
    var node;

    if (!array.length) {
      return result;
    }

    node = nodes.pop();

    do {
      if (toString.call(node) === arrayTypeStr) {
        nodes.push.apply(nodes, node);
      } else {
        result.push(node);
      }
    } while (nodes.length && (node = nodes.pop()) !== undefined);

    result.reverse(); // we reverse result to restore the original order
    return result;
  };

  veda.Util.queryFromIndividual = function (individual) {
    var query;
    var flat = flattenIndividual(individual.properties);
    var allProps = Object.getOwnPropertyNames(flat)
      .map(function (property_uri) {
        if (property_uri === "@" || property_uri === "v-s:isDraft") { return }
        var values = flat[property_uri].sort(function compare(a, b) {
          return a.data < b.data ? - 1 : a.data === b.data ? 0 : 1;
        });
        var oneProp;
        switch (values[0].type) {
          case "Integer":
          case "Decimal":
            oneProp = "'" + property_uri + "'==[" + values[0].data + "," + values[values.length-1].data + "]";
            break;
          // Date
          case "Datetime":
            var start = new Date(values[0].data);
            var end = new Date(values[values.length-1].data);
            start.setHours(0,0,0,0);
            end.setHours(23,59,59,999);
            oneProp = "'" + property_uri + "'==[" + start.toISOString() + "," + end.toISOString() + "]";
            break;
          case "Boolean":
            oneProp = values
              .map( function (value) {
                return "'" + property_uri + "'=='" + value.data + "'";
              })
              .join("||");
            break;
          case "String":
            oneProp = values
              .filter(function(item){return !!item && !!item.valueOf();})
              .map( function (value) {
                //return "'" + property_uri + "'=='" + value.data + "*'";
                var q = value.data;
                if ( !q.match(/[\+\-\*]/) ) {
                  q = q.split(" ")
                       .filter(function (token) { return token.length > 0; })
                       .map(function (token) { return "+" + token + "*"; })
                       .join(" ");
                }
                return "'" + property_uri + "'=='" + q + "'";
              })
              .join("||");
            break;
          case "Uri":
            oneProp = values
              .filter(function(item){return !!item && !!item.valueOf();})
              .map( function (value) {
                if (property_uri === "rdf:type") {
                  return "'" + property_uri + "'==='" + value.data + "'";
                } else {
                  return "'" + property_uri + "'=='" + value.data + "'";
                }
              })
              .join("||");
            break;
        }
        return oneProp ? "(" + oneProp + ")" : undefined;
      })
      .filter(function(item){return typeof item !== undefined;})
      .join("&&");
    query = allProps ? "(" + allProps + ")" : undefined;
    return query;
  }

  function flattenIndividual(object, prefix, union, depth) {
    if (typeof union === "undefined") {
      union = {};
    }
    if (typeof prefix === "undefined") {
      prefix = "";
    }
    if (typeof depth === "undefined") {
      depth = 0;
    }
    if (depth === 5) {
      return;
    }
    for (var property_uri in object) {
      if (property_uri === "@") { continue; }
      var values = object[property_uri];
      var prefixed = prefix ? prefix + "." + property_uri : property_uri;
      for (var i = 0; i < values.length; i++) {
        var value = values[i];
        if (value.type === "Uri") {
          var individ = new veda.IndividualModel(value.data);
          //if ( true ) {
          if ( individ.isNew() ) {
            flattenIndividual(individ.properties, prefixed, union, depth+1);
          } else {
            union[prefixed] = union[prefixed] ? union[prefixed] : [];
            union[prefixed].push( value );
          }
        } else {
          union[prefixed] = union[prefixed] ? union[prefixed] : [];
          union[prefixed].push( value );
        }
      }
    }
    return union;
  }

  veda.Util.transform = function (individual, template, transformId, modal) {
    var startForm = veda.Util.buildStartFormByTransformation(individual, new veda.IndividualModel(transformId));
    if (modal) {
      veda.Util.showModal(startForm, undefined, 'edit');
    } else {
      //startForm.present('#main', undefined, 'edit');
      riot.route("#/" + startForm.id + "///edit");
    }
  }

  /**
   * Event `send` handler:
   *  - Find transformation to start form or use transformation specified by `transformId` parameter
   *  - Apply transformation and redirect to start form.
   */
  veda.Util.send = function (individual, template, transformId, modal) {
    if ( transformId ) {
      if ( !individual.isSync() ) template.trigger('save');
      var startForm = veda.Util.buildStartFormByTransformation(individual, new veda.IndividualModel(transformId));
      veda.Util.showModal(startForm, undefined, 'edit');
    } else {
      individual["v-wf:hasStatusWorkflow"] = [ new veda.IndividualModel("v-wf:ToBeSent") ];
      var results = query(veda.ticket, "'rdf:type' == 'v-s:DocumentLinkRules' && 'v-s:classFrom' == '" + individual["rdf:type"][0].id + "'").result;
      if ( results.length === 0 ) {
        $("#send.action", template).remove();
        $("#edit.action", template).remove();
        $("#save.action", template).remove();
        $("#cancel.action", template).remove();
        $("#delete.action", template).remove();
        template.trigger('save');
        template.closest(".modal").modal("hide").remove();
        veda.trigger("success", {status: "Успешно отправлено / Successfully sent"});
      } else if ( results.length === 1 ) {
        template.trigger('save');
        results.forEach( function (res_id) {
          var res = new veda.IndividualModel(res_id);
          var startForm = veda.Util.buildStartFormByTransformation(individual, res['v-s:hasTransformation'][0]);
          veda.Util.showModal(startForm, undefined, 'edit');
        });
      } else {
        var sendDropdown = $('[resource="' + individual.id + '"] #send + .dropdown-menu');
        sendDropdown.addClass('dropup').addClass('dropdown-toggle');
        if (sendDropdown.html() === '') {
          results.forEach( function (res_id) {
            var res = new veda.IndividualModel(res_id);
            $("<li/>", {
              "style": "cursor:pointer",
              "html" : "<a>" + new veda.IndividualModel(res_id)['rdfs:label'][0] + "</a>",
              "click": (function (e) {
                veda.Util.send(individual, template, res['v-s:hasTransformation'][0].id);
              })
            }).appendTo(sendDropdown);
          });
        }
      }
    }
  }

  /**
   * @returns veda.IndividualModel - start form
   */
  veda.Util.buildStartFormByTransformation = function (individual, transform) {
    var transfromResult = veda.Util.applyTransform(individual, transform);
    var startForm = new veda.IndividualModel();
    Object.getOwnPropertyNames(transfromResult[0]).forEach(function (key)
    {
      if (key !== '@')
      {
        if (!Array.isArray(transfromResult[0][key])) {
          transfromResult[0][key] = [transfromResult[0][key]];
        }
        for (var i in transfromResult[0][key])
        {
          var value = null;
          if (key === 'rdf:type')
          {
            value = new veda.IndividualModel(transfromResult[0][key][i].data);
          } else
          {
            if (transfromResult[0][key][i].type == _Uri) {
              value = new veda.IndividualModel(transfromResult[0][key][i].data);
            } else if (transfromResult[0][key][i].type == _Datetime) {
              value = new Date(Date.parse(transfromResult[0][key][i].data));
            } else {
              value = transfromResult[0][key][i].data;
            }
          }
          if (value) {
            startForm[key] = startForm[key].concat(value);
          }
        }
      }
    });
    return startForm;
  }

  /**
   * Event `createReport` handler:
   *  - Find available reports or use report specified by `reportId` parameter.
   *  - Let user to choice report (if more then one founded)
   *  - Redirect to report
   */
  veda.Util.createReport = function (individual, reportId) {
    if (reportId !== undefined) {
      $('[resource="'+individual.id+'"]').find("#createReport").dropdown('toggle');
      veda.Util.redirectToReport(individual, reportId);
    } else {
      var s = new veda.SearchModel("'rdf:type' == 'v-s:ReportsForClass' && 'v-ui:forClass' == '"+individual["rdf:type"][0].id+"'", null);
      if (Object.getOwnPropertyNames(s.results).length == 0) {
        alert('Нет отчета. Меня жизнь к такому не готовила.');
      } else if (Object.getOwnPropertyNames(s.results).length == 1) {
        $('[resource="'+individual.id+'"]').find("#createReport").dropdown('toggle');
        veda.Util.redirectToReport(individual, Object.getOwnPropertyNames(s.results)[0]);
      } else {
        var reportsDropdown = $('[resource="'+individual.id+'"] #chooseReport + .dropdown-menu');
        if (reportsDropdown.html()== '') {
          Object.getOwnPropertyNames(s.results).forEach( function (res_id) {
            $("<li/>", {
                 "style" : "cursor:pointer",
                       "html" : "<a href='#'>"+new veda.IndividualModel(res_id)['rdfs:label'][0]+"</a>",
                       "click": (function (e) {
                        veda.Util.redirectToReport(individual, res_id);
                       })
                      }).appendTo(reportsDropdown);
          });
        }
      }
    }
  }

  veda.Util.redirectToReport = function (individual, reportId) {
    var jasperServer = new veda.IndividualModel('cfg:jasperServerAddress');
    var jasperServerAddress = jasperServer['rdf:value'][0];
    var report = new veda.IndividualModel(reportId);

    var form = document.createElement("form");
    form.setAttribute("method", "post");
    form.setAttribute("action", jasperServerAddress+'flow.html?_flowId=viewReportFlow&j_username=joeuser&j_password=joeuser&reportUnit='+encodeURIComponent(report['v-s:filePath'][0])+'&output='+encodeURIComponent(report['v-s:fileFormat'][0])+'&documentId='+encodeURIComponent(individual.id)+'&ticket='+veda.ticket);
    form.setAttribute("target", "view");

    Object.getOwnPropertyNames(individual.properties).forEach(function (key)
    {
      if ( key !== '@' && individual.hasValue(key) ) {
        var hiddenField = document.createElement("input");
        hiddenField.setAttribute("type", "hidden");
        hiddenField.setAttribute("name", key.replace(':','_'));
        var value = '';
        individual[key].forEach(function(item, i, arr) {
            if (i>0) value+=',';
            value+=(individual[key][i] instanceof veda.IndividualModel)?individual[key][i].id:individual[key][i];
        });
        hiddenField.setAttribute("value", value);
        form.appendChild(hiddenField);
      }
    });
    document.body.appendChild(form);

    window.open('', 'view');

    form.submit();
  }

  /**
   * Event `showRights` handler:
   *  - Find available reports
   *  - Let user to choice report (if more then one founded)
   *  - Redirect to report
   */
  veda.Util.showRights = function (individual) {
    // Ignore individuals without id
    if (individual.id === undefined || individual.id === '' || individual.id === '_') return;
    var container = $($("#show-rights-modal-template").html());
    container.modal();

    $("body").append(container);

    var rights = individual['rights'];
    var holder = $("<div>");
    rights.present(holder);
    holder.appendTo($(".modal-body", container));

    var origin = individual['rightsOrigin'];
    origin.forEach(function (rightRecord) {
      var holder = $("<div>");
      rightRecord.present(holder);
      holder.appendTo($(".modal-body", container));
    });
  }

  veda.Util.showModal = function (individual, template, mode) {
    var modal = $( $("#notification-modal-template").html() );
    modal.modal();
    $("body").append(modal);
    var container = $(".modal-body", modal);
    individual.present(container, template, mode);
    $(".action#cancel", modal).click(function () {
      modal.modal("hide").remove();
    });
    return modal;
  }

  veda.Util.showMessage = function (message, cssClass, timeout, redirectIndividual, redirectIndividualMode) {
    var container = $($("#notification-modal-template").html());
    container.modal();
    $("body").append(container);
    var $notification = $("<div/>", {
      'html': message,
      'class': cssClass
    });
    $notification.appendTo($(".modal-body", container));
    $notification.click(redirectAfterTimeout);

    setTimeout( function () {
      redirectAfterTimeout();
    }, timeout);

    var redirectAlreadyCalled = false;
    function redirectAfterTimeout() {
      if (redirectAlreadyCalled) return;
      redirectAlreadyCalled = true;
      $('.modal').modal('hide');
      if (redirectIndividual) {
        var main = $('#main');
        main.empty();
        if (typeof redirectIndividual === 'string') {
          redirectIndividual = new veda.IndividualModel(redirectIndividual, undefined, undefined, undefined, false);
        }
        redirectIndividual.present(main, undefined, redirectIndividualMode);
      }
    }
  }

  /**
   * Check that element inside root hierarchy.
   */
  veda.Util.inSubHierarchy = function (root, element) {
      if (typeof element === 'string') {
          element = new veda.IndividualModel(element, undefined, undefined, undefined, false);
      }
      if (element && element.hasValue('rdf:type') && element['rdf:type'][0].id == 'v-s:Department') {
          if (element.id == root || (element.hasValue(['v-s:parentUnit']) && element['v-s:parentUnit'][0].id == root)) {
              return true; // Found
          } else {
              if (element.hasValue(['v-s:parentUnit'])) {
                  return veda.Util.inSubHierarchy(root, element['v-s:parentUnit'][0]); // Check parent
              } else {
                  return false; // No parent
              }
          }
      } else {
          return false; // Not a department
      }
  }
});
