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
  };

  veda.Util.processQuery = function (q, limit, delta, pause, fn) {
    console.log((new Date()).toISOString(), "Process query results |||", "query:", q, " | ", "limit:", limit, " | ", "delta:", delta, " | ", "pause:", pause);
    var result = [], append = [].push, fetchingProgress = 0, processingProgress = 0;
    console.time("Fetching total");
    fetchResult();
    return;

    function fetchResult(cursor) {
      var from = cursor || 0;
      veda.Backend.query({
        ticket: veda.ticket,
        query: q,
        sort: "'v-s:created' desc",
        from: from,
        top: delta,
        limit: limit
      }).then(function (query_result) {
        var cursor = query_result.cursor;
        var estimated = query_result.estimated;
        if ( limit > estimated ) {
          limit = estimated;
        }
        append.apply(result, query_result.result);
        if ( cursor/limit - fetchingProgress >= 0.05 ) {
          fetchingProgress = cursor/limit;
          console.log("Fetching progress:", Math.floor(fetchingProgress * 100) + "%", "(" + cursor, "of", limit + ")");
        }
        if ( cursor === estimated || cursor >= limit ) {
          console.log((new Date()).toString(), "Fetching done:", limit);
          console.timeEnd("Fetching total");
          result.splice(limit - cursor || limit); // cut result to limit
          console.time("Processing total");
          processResult(result);
        } else {
          fetchResult(query_result.cursor);
        }
      });
    }
    function processResult(result) {
      var portion = result.splice(-delta);
      portion.forEach( fn );
      if ( (limit - result.length) / limit - processingProgress >= 0.05 ) {
        processingProgress = (limit - result.length) / limit;
        console.log("Processing progress:", Math.floor(processingProgress * 100) + "%", "(" + (limit - result.length), "of", limit + ")");
      }
      if ( result.length ) {
        setTimeout(processResult, pause, result);
      } else {
        console.log("Processing done:", limit);
        console.timeEnd("Processing total");
      }
    }
  };

  // Escape function for css (jQuery) selectors
  veda.Util.escape4$ = function (str) {
    if (str) return str.replace(/([ #;?%&,.+*~\':"!^$[\]()=>|\/@])/g,'\\$1');
    return str;
  };

  veda.Util.genUri = function () {
    var uid = veda.Util.guid(), re = /^\d/;
    return (re.test(uid) ? "d:a" + uid : "d:" + uid);
  };
  veda.Util.guid = function () {
    return veda.Util.s4() + veda.Util.s4() + veda.Util.s4() + veda.Util.s4() + veda.Util.s4() + veda.Util.s4() + veda.Util.s4() + veda.Util.s4();
  };
  veda.Util.s4 = function () {
    return Math.floor((1 + Math.random()) * 0x10000)
      .toString(36)
      .substring(1);
  };

  veda.Util.mlstring = function (ru, en) {
      var str_ru = new String(ru);
      str_ru.language = "RU";

      var str_en = new String(en);
      str_en.language = "EN";

      return [str_ru, str_en];
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
        var notify = veda.Notify ? new veda.Notify() : function () {};
        notify("danger", error);
        return uri;
      }
    }

    individualList.forEach(function (individual) {
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

  veda.Util.applyTransform = function (individualList, transform) {
    return transformation(veda.ticket, individualList, transform, null, null);
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

  veda.Util.unique = function (arr) {
    var n = {}, r = [];
    for(var i = 0; i < arr.length; i++) {
      if (!n[arr[i]]) {
        n[arr[i]] = true;
        r.push(arr[i]);
      }
    }
    return r;
  }

  veda.Util.queryFromIndividual = function (individual) {
    var query;
    var flat = flattenIndividual(individual.properties);
    if ( individual.hasValue("*") && individual.get("*")[0].indexOf("==") > 0 ) {
      return individual.get("*")[0];
    }
    var allProps = Object.getOwnPropertyNames(flat)
      .map(function (property_uri) {
        if (property_uri === "@") { return }
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
              .join(" || ");
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
              .join(" || ");
            break;
          case "Uri":
            oneProp = values
              .filter(function(item){return !!item && !!item.valueOf();})
              .map( function (value) {
                if (property_uri === "rdf:type") {
                  return "'" + property_uri + "'=='" + value.data + "'";
                } else {
                  return "'" + property_uri + "'=='" + value.data + "'";
                }
              })
              .join(" || ");
            break;
        }
        return oneProp ? "( " + oneProp + " )" : undefined;
      })
      .filter(function(item){return typeof item !== undefined;})
      .join(" && ");
    query = allProps ? "( " + allProps + " )" : undefined;
    return query;
  }

  function flattenIndividual(object, prefix, union, visited) {
    var uri = object["@"];
    union = typeof union !== "undefined" ? union : {};
    prefix = typeof prefix !== "undefined" ? prefix : "";
    visited = typeof visited !== "undefined" ? visited : [];
    if (visited.indexOf(uri) > -1) {
      return;
    } else {
      visited.push(uri);
    }
    for (var property_uri in object) {
      if (property_uri === "@") { continue; }
      var values = object[property_uri];
      var prefixed = prefix ? prefix + "." + property_uri : property_uri;
      for (var i = 0; i < values.length; i++) {
        var value = values[i];
        if (value.type === "Uri") {
          var individ = new veda.IndividualModel(value.data);
          if ( individ.isNew() ) {
            flattenIndividual(individ.properties, prefixed, union, visited);
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
      template.trigger("save");
      return new veda.IndividualModel(transformId).load().then(function (transform) {
        var startForm = veda.Util.buildStartFormByTransformation(individual, new veda.IndividualModel(transformId));
        veda.Util.showModal(startForm, undefined, "edit");
      });
    } else {
      individual["v-wf:hasStatusWorkflow"] = [ new veda.IndividualModel("v-wf:ToBeSent") ];
      template.trigger("save");
      template.closest(".modal").modal("hide").remove();
      var notify = veda.Notify ? new veda.Notify() : function () {};
      notify("success", {name: "Успешно отправлено / Successfully sent"});
    }
  }

  /**
   * @returns veda.IndividualModel - start form
   */
  veda.Util.buildStartFormByTransformation = function (individual, transform) {
    var transfromResult = veda.Util.applyTransform(individual.properties, transform.properties);
    var startForm = new veda.IndividualModel(transfromResult[0]);
    startForm.isNew(true);
    startForm.isSync(false);
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
      veda.Util.redirectToReport(individual, reportId);
    } else {
      var s = veda.Backend.query(veda.ticket, "'rdf:type' == 'v-s:ReportsForClass' && 'v-ui:forClass' == '"+individual["rdf:type"][0].id+"'").then(function (queryResult) {
        var reportUris = queryResult.result;
        if (reportUris.length === 0) {
          alert("Нет отчета. Меня жизнь к такому не готовила.");
        } else if (reportUris.length === 1) {
          veda.Util.redirectToReport(individual, reportUris[0]);
        } else {
          var reportsDropdown = $('[resource="'+individual.id+'"] #chooseReport + .dropdown-menu');
          if (reportsDropdown.html() === "") {
            var reportPromises = reportUris.map(function (reportUri) {
              return new veda.IndividualModel(reportUri).load();
            });
            Promise.all(reportPromises).then(function (reports) {
              reports.forEach(function (report) {
                $("<li/>", {
                  "style" : "cursor:pointer",
                  "html" : "<a href='#'>" + report["rdfs:label"].join(" ") + "</a>",
                  "click": (function (e) {
                    veda.Util.redirectToReport(individual, report.id);
                  })
                }).appendTo(reportsDropdown);
              });
            });
          }
        }
      });
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
        individual.get(key).forEach(function(item, i, arr) {
          if (i>0) value+=',';
          value += (
            item instanceof veda.IndividualModel ? item.id :
            item instanceof Date ? item.toISOString() :
            item
          );
        });
        console.log(key, value);
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
    console.log("timezone", tz);

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
    var modalTmpl = $("#individual-modal-template").html();
    var modal = $(modalTmpl);
    var modalBody = $(".modal-body", modal);
    modal.on("remove", function (e) {
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
          redirectIndividual = new veda.IndividualModel({uri: redirectIndividual, cache: false});
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
          element = new veda.IndividualModel({uri: element, cache: false});
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
