// Browser-side utility functions

veda.Module(function Util(veda) { "use strict";

  veda.Util = veda.Util || {};

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
        formatted = value.toString();
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
    if (ftime === "00:00:00") return fdate;
    return [fdate, ftime].join(" ");
  };
  veda.Util.formatNumber = function (n) {
    return (n+"").replace(/.(?=(?:[0-9]{3})+\b)/g, '$& ');
  };

  veda.Util.toTTL = function (individualList, callback) {
    var s = new veda.SearchModel("'rdf:type'=='owl:Ontology'", null);
    var prefixes = {};
    prefixes["dc"] = "http://purl.org/dc/elements/1.1/";
    prefixes["grddl"] = "http://www.w3.org/2003/g/data-view#";
    Object.getOwnPropertyNames(s.results).map( function (res_id) {
      var res = s.results[res_id];
      prefixes[res_id.substring(0,res_id.length-1)] = res["v-s:fullUrl"][0].toString();
    });
    var writer = N3.Writer({ prefixes: prefixes });
    individualList.each(function (individual) {
      var triple = {};
      if (individual.id.indexOf(":") == individual.id.length-1) {
        triple.subject = prefixes[individual.id.substring(0, individual.id.length - 1)];
      } else {
        triple.subject = N3.Util.expandPrefixedName(individual.id, prefixes);
      }
      // rdf:type first!
      triple.predicate = N3.Util.expandPrefixedName("rdf:type", prefixes);
      individual["rdf:type"].map(function (value) {
        if (value.id.indexOf(":") == value.id.length-1) {
          triple.object = prefixes[value.id.substring(0, value.id.length - 1)];
        } else {
          triple.object = N3.Util.expandPrefixedName(value.id, prefixes);
        }
        writer.addTriple(triple);
      });
      Object.getOwnPropertyNames(individual.properties).map(function (property_uri) {
        if (property_uri === "@") { return; }
        if (property_uri === "rdf:type") { return; }
        triple.predicate = N3.Util.expandPrefixedName(property_uri, prefixes);
        individual[property_uri].map(function (value) {
          if (value instanceof Number || typeof value === "number" ) {
            triple.object = isInteger(value.valueOf()) ? '"' + value.valueOf() + '"^^' + N3.Util.expandPrefixedName('xsd:integer', prefixes) : '"' + value.valueOf() + '"^^' + N3.Util.expandPrefixedName('xsd:decimal', prefixes);
          } else if (value instanceof Boolean || typeof value === "boolean") {
            triple.object = '"' + value.valueOf() + '"^^' + N3.Util.expandPrefixedName("xsd:boolean", prefixes);
          } else if (value instanceof String || typeof value === "string") {
            triple.object = value.language ? '"' + value.valueOf() + '"@' + value.language.toLowerCase() : '"' + value.valueOf() + '"^^' + N3.Util.expandPrefixedName("xsd:string", prefixes);
          } else if (value instanceof Date) {
            triple.object = '"' + value.toISOString() + '"^^' + N3.Util.expandPrefixedName("xsd:dateTime", prefixes);
          } else if (value instanceof veda.IndividualModel) {
            if (value.id.indexOf(":") == value.id.length-1) {
              triple.object = prefixes[value.id.substring(0, value.id.length - 1)];
            } else {
              triple.object = N3.Util.expandPrefixedName(value.id, prefixes);
            }
          }
          writer.addTriple(triple);
        });
      });
    });
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
    // Serialize individual as search query
    var query;
    var allProps = Object.getOwnPropertyNames(individual.properties)
      .map(function (property_uri) {
        if (property_uri === "@") { return }
        var property = new veda.IndividualModel(property_uri);
        var values = individual[property_uri];//.filter(function(item){return !!item && !!item.valueOf();});
        // Filter rdfs:Resource type
        if (property_uri === "rdf:type") {
          values = individual[property_uri].filter(function(item){ return item.id !== "rdfs:Resource" });
        }
        var oneProp;
        switch (property["rdfs:range"][0].id) {
          case "xsd:integer":
          case "xsd:nonNegativeInteger":
          case "xsd:decimal":
            oneProp =
              values.length === 1 ? "'" + property_uri + "'==[" + values[0] + "," + values[0] + "]" :
              values.length > 1 ? "'" + property_uri + "'==[" + values[0] + "," + values[values.length-1] + "]" :
              undefined;
            break;
          case "xsd:dateTime":
              if (values.length === 1) {
                var start = values[0].toISOString().substring(0,19);
                values[0].setHours(23, 59, 59, 999);
                var end = values[0].toISOString().substring(0,19);
                oneProp = "'" + property_uri + "'==[" + start + "," + end + "]";
              } else if (values.length > 1) {
                values[values.length-1].setHours(23, 59, 59, 999);
                oneProp = "'" + property_uri + "'==[" + values[0].toISOString().substring(0,19) + "," + values[values.length-1].toISOString().substring(0,19) + "]";
              } else {
                oneProp = undefined;
              }
            break;
          case "xsd:boolean":
            oneProp = values
              //.filter(function(item){return !!item && !!item.valueOf();})
              .map( function (value) {
                return "'" + property_uri + "'=='" + value + "'";
              })
              .join("||");
            break;
          case "xsd:string":
          case "rdfs:Literal":
            oneProp = values
              .filter(function(item){return !!item && !!item.valueOf();})
              .map( function (value) {
                return "'" + property_uri + "'=='" + value + "*'";
              })
              .join("||");
            break;
          default:
            oneProp = values
              .filter( function (value) {
                return value instanceof veda.IndividualModel;
              })
              .map( function (value) {
                return "'" + property_uri + "'=='" + value.id + "'";
              })
              .join("||");
            break;
        }
        return oneProp ? "(" + oneProp + ")" : undefined;
      })
      .filter(function(item){return !!item;})
      .join("&&");
    query = allProps ? "(" + allProps + ")" : undefined;
    return query;
  }

  veda.Util.transform = function (individual, template, transformId, modal) {
    var startForm = veda.Util.buildStartFormByTransformation(individual, new veda.IndividualModel(transformId));
    if (modal) {
      veda.Util.showModal(startForm, 'edit');
    } else {
      startForm.present('#main', undefined, 'edit');
    }
  }

  /**
   * Event `send` handler:
   *  - Find transformation to start form or use transformation specified by `transformId` parameter
   *  - Apply transformation and redirect to start form.
   */
  veda.Util.send = function (individual, template, transformId, modal) {
    if (typeof modal == 'undefined') {
      modal = false;
    }

    individual["v-wf:hasStatusWorkflow"] = [ new veda.IndividualModel("v-wf:ToBeSent") ];
    //$('[resource="'+individual.id+'"]').find("#save").trigger("click");
    template.trigger('save');
    if (individual.redirectToIndividual) {
      individual = individual.redirectToIndividual;
    }

    if (transformId !== undefined) {
      var startForm = veda.Util.buildStartFormByTransformation(individual, new veda.IndividualModel(transformId));
      if (modal) {
        veda.Util.showModal(startForm, 'edit');
      } else {
        riot.route( ["#", startForm.id, '#main', undefined, "edit"].join("/") );
        //startForm.present('#main', undefined, 'edit');
      }
    } else {
      var s = new veda.SearchModel("'rdf:type' == 'v-s:DocumentLinkRules' && 'v-s:classFrom' == '"+individual["rdf:type"][0].id+"'", null);
      if (Object.getOwnPropertyNames(s.results).length == 0) {
        var individualNode = $('[resource="'+individual.id+'"]');
        individualNode.find("#send.action").remove();
        individualNode.find("#edit.action").remove();
        individualNode.find("#save.action").remove();
        individualNode.find("#cancel.action").remove();
        individualNode.find("#delete.action").remove();
        if (individual.is('v-wf:StartForm') || individual.hasValue('v-wf:processedDocument') || individual.hasValue('v-wf:onDocument')) {
          individual.trigger("individual:afterSend");
          if (individual.sendConfirmed != true) {
            veda.Util.showMessage("<div class='row'><div class='col-md-12'><br><br><h2>"+new veda.IndividualModel("v-s:WillBeProcessed")['rdfs:label'][0]+"</h2></div></div>", "", 5000,
              individual.is('v-wf:StartForm')?individual.id:
              individual.hasValue('v-wf:processedDocument')?individual['v-wf:processedDocument'][0].id:individual['v-wf:onDocument'][0].id, "view");
          }
        }
      } else if (Object.getOwnPropertyNames(s.results).length == 1) {
        $('[resource="'+individual.id+'"]').find("#save").trigger("click");
        Object.getOwnPropertyNames(s.results).forEach( function (res_id) {
          var res = s.results[res_id];
          var startForm = veda.Util.buildStartFormByTransformation(individual, res['v-s:hasTransformation'][0]);
          if (modal) {
            veda.Util.showModal(startForm, 'edit');
          } else {
                  riot.route("#/" + startForm.id + "///edit", true);
          }
        });
      } else {
        var sendDropdown = $('[resource="'+individual.id+'"] #send + .dropdown-menu');
        sendDropdown.addClass('dropup').addClass('dropdown-toggle');
        if (sendDropdown.html()== '') {
          Object.getOwnPropertyNames(s.results).forEach( function (res_id) {
            var res = s.results[res_id];
            $("<li/>", {
                 "style" : "cursor:pointer",
                       "html" : "<a>"+new veda.IndividualModel(res_id)['rdfs:label'][0]+"</a>",
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
    form.setAttribute("action", jasperServerAddress+'flow.html?_flowId=viewReportFlow&j_username=joeuser&j_password=joeuser&reportUnit='+encodeURIComponent(report['v-s:filePath'][0])+'&output='+encodeURIComponent(report['v-s:fileFormat'][0])+'&documentId='+encodeURIComponent(individual.id));
    form.setAttribute("target", "view");

    Object.getOwnPropertyNames(individual.properties).forEach(function (key)
    {
      if (key !== '@') {
          var hiddenField = document.createElement("input");
          hiddenField.setAttribute("type", "hidden");
          hiddenField.setAttribute("name", key.replace(':','_'));
          hiddenField.setAttribute("value", (individual[key][0] instanceof veda.IndividualModel)?individual[key][0].id:individual[key][0]);
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


  veda.Util.showModal = function (individual, mode) {
    if (typeof mode == 'undefined') {
      mode = 'view';
    }

    // Ignore individuals without id
    if (individual.id === undefined || individual.id === '' || individual.id === '_') return;
    var container = $($("#notification-modal-template").html());
    container.modal();

    $("body").append(container);

    var holder = $("<div>");
    individual.present(holder, undefined, mode);
    holder.appendTo($(".modal-body", container));
  }

  veda.Util.showMessage = function (message, cssClass, timeout, redirectIndividual, redirectIndividualMode) {
    var container = $($("#notification-modal-template").html());
    container.modal();

    $("body").append(container);

    var redirectAlreadyCalled = false;
    function redirectAfterTimeout() {
      if (redirectAlreadyCalled) return;
      redirectAlreadyCalled = true;
      $('.modal').modal('hide');
      var main = $('#main');
      main.empty();
      if (typeof redirectIndividual === 'string') {
        redirectIndividual = new veda.IndividualModel(redirectIndividual, undefined, undefined, undefined, false);
      }
      redirectIndividual.present(main, undefined, redirectIndividualMode);
    }

    var $notification = $("<div/>", {
      'html': message,
      'class': cssClass
    });
    $notification.appendTo($(".modal-body", container));
    $notification.on("click", function() {redirectAfterTimeout();});

    setTimeout( function () {
      redirectAfterTimeout();
    }, timeout);
  }
});
