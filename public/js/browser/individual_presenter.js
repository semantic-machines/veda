// Individual Presenter

veda.Module(function IndividualPresenter(veda) { "use strict";

  veda.IndividualModel.prototype.present = function (container, template, mode, extra) {

    try {

      // Defaults
      container = container || "#main";
      mode = mode || "view";

      if (typeof container === "string") {
        container = $(container).empty();
      }

      if (container.prop("id") === "main") { container.hide(); }

      template = present(this, container, template, mode, extra);

      if (container.prop("id") === "main") { container.show("fade", 250); }

    } catch (err) {
      console.log(err);
    }

    return template;
  };

  function present(individual, container, template, mode, extra) {

    var ontology = new veda.OntologyModel();

    var specs = $.extend.apply (
      {}, [].concat(
        individual["rdf:type"].map( function (_class) {
          return ontology.getClassSpecifications(_class.id);
        })
      )
    );

    if (template) {
      if (typeof template === "string" && (/^(\w|-)+:.*?$/).test(template) ) {
        template = new veda.IndividualModel(template);
      } else if (typeof template === "string") {
        var templateString = template;
        var uri = veda.Util.hash(templateString).toString();
        template = veda.cache[uri] ? veda.cache[uri] : new veda.IndividualModel({
          "@": uri,
          "v-ui:template": [{data: templateString, type: "String"}]
        });
      } else if (template instanceof HTMLElement) {
        var templateString = template.outerHTML;
        var uri = veda.Util.hash(templateString).toString();
        template = veda.cache[uri] ? veda.cache[uri] : new veda.IndividualModel({
          "@": uri,
          "v-ui:template": [{data: templateString, type: "String"}]
        });
      }
      return renderTemplate(individual, container, template, mode, extra, specs);
    } else {
      var isClass = individual.hasValue("rdf:type", "owl:Class") || individual.hasValue("rdf:type", "rdfs:Class");
      if ( individual.hasValue("v-ui:hasTemplate") && !isClass ) {
        template = individual["v-ui:hasTemplate"][0];
        return renderTemplate(individual, container, template, mode, extra, specs);
      } else {
        return individual["rdf:type"].reduce(function (acc, type) {
          if ( type.hasValue("v-ui:hasTemplate") ) {
            template = type["v-ui:hasTemplate"][0];
          } else {
            template = new veda.IndividualModel("v-ui:generic");
          }
          var renderedTemplate = renderTemplate(individual, container, template, mode, extra, specs);
          return acc.add(renderedTemplate);
        }, $());
      }
    }
  }

  function renderTemplate(individual, container, template, mode, extra, specs) {

    var templateString, preScript, postScript;

    if ( template.preprocessed ) {
      preScript = template.preScript;
      templateString = template.templateString;
      postScript = template.postScript;
    } else {
      templateString = template["v-ui:template"][0].toString().trim();
      var match,
          preScript_src,
          preScript,
          postScript_src,
          postScript;

      // Extract pre script, template and post script
      // match = template.match(/^(?:<script[^>]*>([\s\S]*?)<\/script>)?([\s\S]*?)(?:<script[^>]*>(?![\s\S]*<script[^>]*>)([\s\S]*)<\/script>)?$/i);
      match = preProcess(templateString);
      preScript_src = match[1];
      templateString = match[2];
      postScript_src = match[3];

      if (preScript_src) {
        preScript = new Function("veda", "individual", "container", "template", "mode", "extra", "\"use strict\";" + preScript_src);
        template.preScript = preScript;
      }

      template.templateString = templateString;

      if (postScript_src) {
        postScript = new Function("veda", "individual", "container", "template", "mode", "extra", "\"use strict\";" + postScript_src);
        template.postScript = postScript;
      }

      template.preprocessed = true;
    }

    var renderedTemplate = $(templateString);

    if (preScript) {
      preScript.call(individual, veda, individual, container, renderedTemplate, mode, extra);
    }

    renderedTemplate = processTemplate (individual, container, renderedTemplate, mode, extra, specs);
    container.append(renderedTemplate);

    // Timeout to wait all related individuals to render
    setTimeout(function () {
      renderedTemplate.trigger(mode);
      if (postScript) {
        postScript.call(individual, veda, individual, container, renderedTemplate, mode, extra);
      }
    }, 0);

    // Watch individual updates on server
    var updateService = new veda.UpdateService();
    updateService.subscribe(individual.id);
    renderedTemplate.one("remove", function () {
      updateService.unsubscribe(individual.id);
    });

    // Watch language change
    veda.on("language:changed", localizeIndividual);
    renderedTemplate.one("remove", function () {
      veda.off("language:changed", localizeIndividual);
    });

    function localizeIndividual () {
      for (var property_uri in individual.properties) {
        if (property_uri === "@") { continue; }
        if ( individual.hasValue(property_uri) && individual.properties[property_uri][0].type === "String" ) {
          individual.trigger("propertyModified", property_uri, individual.get(property_uri));
          individual.trigger(property_uri, individual.get(property_uri));
        }
      }
    }

    return renderedTemplate;
  }

  function preProcess(templateString) {
    var pre,
        preStart = templateString.indexOf("<script>") + 8,
        preEnd = templateString.indexOf("</script>"),
        hasPre = preStart === 8;
    if (hasPre) {
      pre = templateString.substring(preStart, preEnd);
      templateString = templateString.substring(preEnd + 9);
    }
    var post,
        postStart = templateString.lastIndexOf("<script>") + 8,
        postEnd = templateString.lastIndexOf("</script>"),
        hasPost = postEnd === templateString.length - 9;
    if (hasPost) {
      post = templateString.substring(postStart, postEnd);
      templateString = templateString.substring(0, postStart - 8);
    }
    // Compatible with regexp
    return [undefined, pre, templateString, post];
  }

  function processTemplate (individual, container, template, mode, extra, specs) {

    template.attr({
      "resource": individual.id,
      "typeof": individual["rdf:type"].map(function (item) { return item.id; }).join(" ")
    });

    // Unwrapped templates support
    var wrapper = $("<div>").append(template);

    var view = $(".view", wrapper);
    var edit = $(".edit", wrapper);
    var search = $(".search", wrapper);
    var _view = $(".-view", wrapper);
    var _edit = $(".-edit", wrapper);
    var _search = $(".-search", wrapper);

    // Apply mode to template to show/hide elements in different modes
    function modeHandler (e) {
      mode = e.type;
      template.data("mode", mode);
      switch (mode) {
        case "view": view.show(); _view.hide(); break;
        case "edit": edit.show(); _edit.hide(); break;
        case "search": search.show(); _search.hide(); break;
      }
      e.stopPropagation();
    }
    template.on("view edit search", modeHandler);

    // Embedded templates list
    var embedded = [];

    // Trigger same events for embedded templates
    function syncEmbedded (e) {
      embedded.map(function (item) {
        item.trigger(e.type, individual.id);
      });
      e.stopPropagation();
    }
    template.on("view edit search save cancel delete recover draft destroy", syncEmbedded);

    // Define handlers
    function saveHandler (e, parent) {
      if (parent !== individual.id) {
        individual.save();
      }
      template.trigger("view");
      e.stopPropagation();
    }
    template.on("save", saveHandler);

    function draftHandler (e, parent) {
      if (parent !== individual.id) {
        individual.draft();
      }
      template.trigger("view");
      e.stopPropagation();
    }
    template.on("draft", draftHandler);

    function cancelHandler (e, parent) {
      if (parent !== individual.id) {
        individual.reset()
          .then( function () {
            if (container.prop("id") === "main") {
              window.history.back();
            }
          }, function () {
            if (container.prop("id") === "main") {
              window.history.back();
            }
          });
      }
      template.trigger("view");
      e.stopPropagation();
    }
    template.on("cancel", cancelHandler);

    // Deleted alert
    function deletedHandler () {
      if ( this.hasValue("v-s:deleted", true) ) {
        if ( container.prop("id") === "main" && !template.hasClass("deleted") ) {
          var alert = new veda.IndividualModel("v-s:DeletedAlert")["rdfs:label"].join(" ");
          var recover = new veda.IndividualModel("v-s:Recover")["rdfs:label"].join(" ");
          var deletedAlert = $(
            '<div id="deleted-alert" class="alert alert-warning no-margin clearfix" role="alert">\
              <p id="deleted-alert-msg">' + alert + '  <button id="deleted-alert-recover" class="btn btn-primary btn-xs recover pull-right">' + recover + '</button></p>\
            </div>'
          );
          template.prepend(deletedAlert);
          $(".recover", deletedAlert).click(function () {
            template.trigger("recover");
          });
        }
        template.addClass("deleted");
      } else {
        template.removeClass("deleted");
        if ( container.prop("id") === "main" ) {
          $("#deleted-alert", template).remove();
        }
      }
    }
    individual.on("v-s:deleted", deletedHandler);
    template.one("remove", function () {
      individual.off("v-s:deleted", deletedHandler);
    });
    deletedHandler.call(individual);

    function deleteHandler (e, parent) {
      if (parent !== individual.id) {
        individual.delete();
      }
      e.stopPropagation();
    }
    template.on("delete", deleteHandler);

    function destroyHandler (e, parent) {
      if (parent !== individual.id) {
        individual.remove();
      }
      e.stopPropagation();
    }
    template.on("destroy", destroyHandler);

    function recoverHandler (e, parent) {
      if (parent !== individual.id) {
        individual.recover();
      }
      e.stopPropagation();
    }
    template.on("recover", recoverHandler);

    // Draft label
    var draftable, showLabel;

    function isDraftHandler() {
      if ( mode === "edit" && draftable && !individual.isSync() ) {
        individual.draft();
      }
      // If individual is draft
      if ( individual.isDraft() && showLabel ) {
        template.addClass("is-draft");
      } else {
        template.removeClass("is-draft");
      }
    }
    individual.on("propertyModified afterSave afterReset", isDraftHandler);
    template.one("remove", function () {
      individual.off("propertyModified", isDraftHandler);
      individual.off("afterSave", isDraftHandler);
      individual.off("afterReset", isDraftHandler);
    });

    setTimeout( function () {
      draftable = individual.is("v-s:UserThing");
      if (draftable) {
        showLabel = !template.parent().closest("[resource='" + individual.id + "']").length;
        if (showLabel) {
          isDraftHandler();
        }
      }
    }, 100);

    // Process RDFa compliant template

    // Special (not RDFa)
    $("[href*='@']:not([rel] *):not([about] *)", wrapper).map( function () {
      var self = $(this);
      var str = self.attr("href");
      self.attr("href", str.replace("@", individual.id));
    });

    $("[src*='@']:not([rel] *):not([about] *)", wrapper).map( function () {
      var self = $(this);
      var str = self.attr("src");
      self.attr("src", str.replace("@", individual.id));
    });

    // Property value
    var props_ctrls = {};
    $("[property]:not(veda-control):not([rel] *):not([about]):not([about] *)", wrapper).map( function () {
      var propertyContainer = $(this),
          property_uri = propertyContainer.attr("property"),
          spec = specs[property_uri] ? new veda.IndividualModel( specs[property_uri] ) : undefined;

      function idModifiedHandler() {
        propertyContainer.text(individual.id);
      }
      if (property_uri === "@") {
        propertyContainer.text(individual.id);
        individual.on("idChanged", idModifiedHandler);
        template.one("remove", function () {
          individual.off(property_uri, idModifiedHandler);
        });
        return;
      }
      renderPropertyValues(individual, property_uri, propertyContainer, props_ctrls, template, mode);

      // Re-render all property values if model's property was changed
      function propertyModifiedHandler() {
        renderPropertyValues(individual, property_uri, propertyContainer, props_ctrls, template, mode);
      }
      individual.on(property_uri, propertyModifiedHandler);
      template.one("remove", function () {
        individual.off(property_uri, propertyModifiedHandler);
      });
    });

    // Fetch related individuals all together
    var prefetch_args = [1];

    // Related resources
    var rels = $("[rel]:not(veda-control):not([rel] *):not([about] *)", wrapper);
    rels.map( function () {
      var rel_uri = $(this).attr("rel");
      if ( individual.hasValue(rel_uri) ) {
        prefetch_args.push(rel_uri);
      }
    });
    if (prefetch_args.length > 1) {
      individual.prefetch.apply(individual, prefetch_args);
    }

    // Fetch about resources alltogether
    var abouts = [];
    $("[about]:not([rel] *):not([about] *)", wrapper).map( function () {
      var about_uri = $(this).attr("about");
      if (about_uri !== "@" && !veda.cache[about_uri] ) {
        abouts.push(about_uri);
      }
    });
    if (abouts.length) {
      get_individuals(veda.ticket, veda.Util.unique(abouts) ).map(function (item) {
        var about = new veda.IndividualModel(item);
      });
    }

    // Related resources & about resources
    rels.map( function () {
      //$("[rel]:not(veda-control):not([rel] *):not([about] *)", wrapper).map( function () {
      var relContainer = $(this),
          about = relContainer.attr("about"),
          rel_uri = relContainer.attr("rel"),
          isEmbedded = relContainer.attr("data-embedded") === "true",
          spec = specs[rel_uri] ? new veda.IndividualModel( specs[rel_uri] ) : undefined,
          rel_inline_template = relContainer.html().trim(),
          rel_template_uri = relContainer.attr("data-template"),
          relTemplate,
          isAbout;

      var sortableOptions = {
        delay: 150,
        placeholder: "sortable-placeholder",
        forcePlaceholderSize: true,
        handle: ".button-drag",
        cancel: "",
        update: function () {
          var uris = $(this).sortable("toArray", {attribute: "resource"});
          individual.set(
            rel_uri,
            uris.map(function (uri) {
              return new veda.IndividualModel(uri);
            })
          );
        }
      };
      relContainer.sortable(sortableOptions);

      if (about) {
        isAbout = true;
        about = (about === "@" ? individual : new veda.IndividualModel(about));
        relContainer.attr("about", about.id);
      } else {
        isAbout = false;
        about = individual;
      }

      if ( rel_template_uri ) {
        relTemplate = new veda.IndividualModel( rel_template_uri );
      } else if ( rel_inline_template.length ) {
        relTemplate = rel_inline_template;
      }
      relContainer.empty();

      template.on("view edit search", function (e) {
        if (e.type === "view") {
          relContainer.sortable("disable");
        } else if (e.type === "edit") {
          relContainer.sortable("enable");
          var property = new veda.IndividualModel(rel_uri);
          if ( isEmbedded
              && spec
              && spec["v-ui:minCardinality"][0] >= 1
              && !individual.hasValue(rel_uri)
              && !(property.hasValue("rdfs:range") && property["rdfs:range"][0].id === "v-s:File")
          ) {
            var valueType = spec && spec.hasValue("v-ui:rangeRestriction") ?
                spec["v-ui:rangeRestriction"] : property.hasValue("rdfs:range") ?
                property["rdfs:range"]        : [];
            var emptyValue = new veda.IndividualModel();
            if ( valueType.length ) {
              emptyValue["rdf:type"] = valueType;
            }
            individual.set(rel_uri, [emptyValue]);
          }
        } else if (e.type === "search") {
          relContainer.sortable("enable");
        }
        e.stopPropagation();
      });

      var values = about.get(rel_uri), rendered = {}, counter = 0;

      propertyModifiedHandler(values);
      about.on(rel_uri, propertyModifiedHandler);
      template.one("remove", function () {
        about.off(rel_uri, propertyModifiedHandler);
      });

      if (isEmbedded) {
        embeddedHandler(values);
        about.on(rel_uri, embeddedHandler);
        template.one("remove", function () {
          about.off(rel_uri, embeddedHandler);
        });
      }

      // Re-render link property if its' values were changed
      function propertyModifiedHandler (values) {
        ++counter;
        try {
          if (values.length) {
            values.map(function (value) {
              if (value.id in rendered) {
                rendered[value.id].cnt = counter;
                return;
              }
              setTimeout (function () {
                var renderedTmpl = renderRelationValue (about, rel_uri, value, relContainer, relTemplate, isEmbedded, embedded, isAbout, template, mode);
                rendered[value.id] = {tmpl: renderedTmpl, cnt: counter};
              }, 0);
            });
          } else {
            relContainer.empty();
          }
        } catch (error) {
          if (error instanceof TypeError) {
            var notify = veda.Notify ? new veda.Notify() : function () {};
            notify("warning", {name: "Error", message: "Attribute undefined: " + rel_uri});
          }
        }
        // Remove rendered templates for removed values
        for (var i in rendered) {
          if (rendered[i].cnt === counter) continue;
          rendered[i].tmpl.remove();
          delete rendered[i];
        }
      }

      function embeddedHandler(values) {
        if (mode === "edit") {
          values.map(function (value) {
            if (
              value.id !== about.id // prevent self parent
              && rel_uri !== "v-s:parent" // prevent circular parent
              && !value.hasValue("v-s:parent") // do not change parent
            ) {
              value["v-s:parent"] = [about];
            }
          });
        }
      }

    });

    // About resource
    $("[about]:not([rel]):not([property])", wrapper).map( function () {
      var aboutContainer = $(this),
          about_template_uri = aboutContainer.attr("data-template"),
          about_inline_template = aboutContainer.html().trim(),
          isEmbedded = aboutContainer.attr("data-embedded") === "true",
          about, aboutTemplate;
      if ( about_template_uri ) {
        aboutTemplate = new veda.IndividualModel( about_template_uri );
      } else if ( about_inline_template.length ) {
        aboutTemplate = about_inline_template;
      }
      aboutContainer.empty();
      if (aboutContainer.attr("about") === "@") {
        about = individual;
        aboutContainer.attr("about", about.id);
      } else {
        about = new veda.IndividualModel(aboutContainer.attr("about"));
      }
      aboutTemplate = about.present(aboutContainer, aboutTemplate);
      if (isEmbedded) {
        aboutTemplate.data("isEmbedded", true);
        embedded.push(aboutTemplate);
      }
    });

    // About resource property
    $("[about][property]:not([rel] *):not([about] *)", wrapper).map( function () {
      var propertyContainer = $(this),
          property_uri = propertyContainer.attr("property"),
          about;
      if (propertyContainer.attr("about") === "@") {
        about = individual;
        propertyContainer.attr("about", about.id);
      } else {
        about = new veda.IndividualModel(propertyContainer.attr("about"));
      }
      propertyModifiedHandler();

      function propertyModifiedHandler() {
        if (property_uri === "@") {
          propertyContainer.text( about.id );
        } else {
          var formatted = about.get(property_uri).map(veda.Util.formatValue).join(" ");
          propertyContainer.text( formatted );
        }
      }
      about.on(property_uri, propertyModifiedHandler);
      template.one("remove", function () {
        about.off(property_uri, propertyModifiedHandler);
      });

      // Watch server-side updates
      var updateService = new veda.UpdateService();
      updateService.subscribe(about.id);
      template.one("remove", function () {
        updateService.unsubscribe(about.id);
      });

      // Watch language change
      veda.on("language:changed", localize);
      template.one("remove", function () {
        veda.off("language:changed", localize);
      });
      function localize() {
        if ( about.hasValue(property_uri) && about.properties[property_uri][0].type === "String" ) {
          about.trigger("propertyModified", property_uri, about.get(property_uri));
          about.trigger(property_uri, about.get(property_uri));
        }
      }
    });

    // Validation with support of embedded templates (arbitrary depth)

    // Initial validation state
    var validation = {state: true};
    template.data("validation", validation);

    function validateTemplate (e) {
      if ( Object.keys(validation).length === 0) {
        if ( !template.data("isEmbedded") ) {
          e.stopPropagation();
        }
        return;
      }
      if (mode === "edit") {
        Object.keys(validation).map( function (property_uri) {
          if (property_uri === "state") { return; }
          var spec = specs[property_uri] ? new veda.IndividualModel( specs[property_uri] ) : undefined;
          validation[property_uri] = validate(individual, property_uri, spec);
        });
        template.trigger("validate");
        validation.state = Object.keys(validation).reduce( function (acc, property_uri) {
          if (property_uri === "state") { return acc; }
          return acc && validation[property_uri].state;
        }, true);
        validation.state = validation.state && embedded.reduce(function (acc, embeddedTemplate) {
          var embeddedValidation = embeddedTemplate.data("validation");
          return embeddedValidation ? acc && embeddedValidation.state : acc;
        }, true);
        template.trigger("internal-validated", [validation]);
      }
      // "validate" event should bubble up to be handled by parent template only if current template is embedded
      if ( !template.data("isEmbedded") ) {
        e.stopPropagation();
      }
    }
    template.on("internal-validate", validateTemplate);

    function triggerValidation() {
      if (mode === "edit") {
        template.trigger("internal-validate");
      }
    };
    individual.on("propertyModified", triggerValidation);
    template.one("remove", function () {
      individual.off("propertyModified", triggerValidation);
    });
    template.on("edit", triggerValidation);

    // Handle validation events from template
    template.on("validate", function (e) {
      e.stopPropagation();
    });
    template.on("validated", function (e, validationResult) {
      e.stopPropagation();
      if (mode === "edit") {
        // Merge template validation results with internal validation results
        Object.keys(validationResult).map(function (property_uri) {
          if (property_uri === "state") { return; }
          validation[property_uri] = validationResult[property_uri];
        });
        validation.state = validation.state && validationResult.state;
      }
    });


    // Property control
    $("veda-control[property]:not([rel] *):not([about] *)", wrapper).map( function () {

      var control = $(this),
          property_uri = control.attr("property"),
          type = control.attr("data-type") || "generic",
          spec = specs[property_uri] ? new veda.IndividualModel( specs[property_uri] ) : undefined,
          controlType = $.fn["veda_" + type];

      //control.removeAttr("property");

      // Initial validation state
      validation[property_uri] = {state: true, cause: []};

      function validatedHandler(e, validation) {
        if ( validation.state || !validation[property_uri] || validation[property_uri].state === true ) {
          control.removeClass("has-error");
          control.popover("destroy");
        } else {
          control.addClass("has-error");
          control.popover({
            content: function () {
              return validation[property_uri].cause.map(function (cause_uri) {
                return (new veda.IndividualModel(cause_uri))["rdfs:comment"].join(", ");
              }).join("\n");
            },
            container: control,
            trigger: "hover focus",
            placement: "top",
            animation: false
          });
          if ( $("input", control).is(":focus") ) {
            control.popover("show");
          }
        }
        e.stopPropagation();
      }
      template.on("internal-validated", validatedHandler);

      template.on("view edit search", function (e) {
        e.stopPropagation();
        control.trigger(e.type);
      });

      function assignDefaultValue (e) {
        if ( spec && spec.hasValue("v-ui:defaultValue") && !individual.hasValue(property_uri) ) {
          individual.set(property_uri, spec["v-ui:defaultValue"]);
        }
        e.stopPropagation();
      }
      template.on("edit", assignDefaultValue);

      var opts = {
        individual: individual,
        property_uri: property_uri,
        spec: spec,
        mode: mode
      };

      controlType.call(control, opts);

      props_ctrls[property_uri] ? props_ctrls[property_uri].push(control) : props_ctrls[property_uri] = [ control ];

    });

    // Relation control
    $("veda-control[rel]:not([rel] *):not([about] *)", wrapper).map( function () {

      var control = $(this),
          rel_uri = control.attr("rel"),
          spec = specs[rel_uri] ? new veda.IndividualModel( specs[rel_uri] ) : undefined,
          type = control.attr("data-type") || "link",
          controlType = $.fn["veda_" + type];

      //control.removeAttr("rel");

      // Initial validation state
      validation[rel_uri] = {state: true, cause: []};

      function validatedHandler(e, validation) {
        if ( validation.state || !validation[rel_uri] || validation[rel_uri].state === true) {
          control.removeClass("has-error");
          control.popover("destroy");
        } else {
          control.addClass("has-error");
          control.popover({
            content: function () {
              return validation[rel_uri].cause.map(function (cause_uri) {
                return (new veda.IndividualModel(cause_uri))["rdfs:comment"].join(", ");
              }).join("\n");
            },
            container: control,
            trigger: "hover focus",
            placement: "top",
            animation: false
          });
          if ( $("input", control).is(":focus") ) {
            control.popover("show");
          }
        }
        e.stopPropagation();
      }
      template.on("internal-validated", validatedHandler);

      template.on("view edit search", function (e) {
        e.stopPropagation();
        control.trigger(e.type);
      });

      function assignDefaultValue (e) {
        if ( spec && spec.hasValue("v-ui:defaultValue") && !individual.hasValue(rel_uri) ) {
          individual.set(rel_uri, spec["v-ui:defaultValue"]);
        }
        e.stopPropagation();
      }
      template.on("edit", assignDefaultValue);

      var opts = {
        individual: individual,
        rel_uri: rel_uri,
        spec: spec,
        mode: mode
      };
      controlType.call(control, opts);

    });

    return template;
  }

  function renderPropertyValues(individual, property_uri, propertyContainer, props_ctrls, template, mode) {
    propertyContainer.empty();
    individual.get(property_uri).map( function (value, i) {
      var valueHolder = $("<span class='value-holder'></span>");
      propertyContainer.append(valueHolder.text( veda.Util.formatValue(value) ));
      var wrapper = $("<div id='prop-actions' class='btn-group btn-group-xs' role='group'></div>");
      var btnEdit = $("<button class='btn btn-default'><span class='glyphicon glyphicon-pencil'></span></button>");
      var btnRemove = $("<button class='btn btn-default'><span class='glyphicon glyphicon-remove'></span></button>");
      wrapper.append(btnEdit, btnRemove);

      template.on("view edit search", function (e) {
        if (e.type === "view") wrapper.hide();
        else wrapper.show();
        e.stopPropagation();
      });
      if (mode === "view") { wrapper.hide(); }

      btnRemove.click(function () {
        individual.set( property_uri, individual.get(property_uri).filter(function (_, j) {return j !== i; }) );
      }).mouseenter(function () {
        valueHolder.addClass("red-outline");
      }).mouseleave(function () {
        valueHolder.removeClass("red-outline");
      });
      btnEdit.click(function () {
        var val;
        individual.set(
          property_uri,
          individual.get(property_uri).filter(function (_, j) {
            var test = j !== i;
            if (!test) val = individual.get(property_uri)[j];
            return test;
          })
        );
        if ( props_ctrls[property_uri] ) {
          props_ctrls[property_uri].map(function (item, i) {
            item.val(val);
            if (i === 0) item.trigger("veda_focus", [val]);
          });
        }
      }).mouseenter(function () {
        valueHolder.addClass("blue-outline");
      }).mouseleave(function () {
        valueHolder.removeClass("blue-outline");
      });
      valueHolder.append( wrapper );
    });
  }

  function renderRelationValue(individual, rel_uri, value, relContainer, relTemplate, isEmbedded, embedded, isAbout, template, mode) {
    var valTemplate = value.present(relContainer, relTemplate, isEmbedded ? mode : "view");
    if (isEmbedded) {
      valTemplate.data("isEmbedded", true);
      embedded.push(valTemplate);
      valTemplate.one("remove", function () {
        if (embedded.length) {
          var index = embedded.indexOf(valTemplate);
          if ( index >= 0 ) embedded.splice(index, 1);
        }
      });
    }
    if (!isAbout) {
      var wrapper = $("<div id='rel-actions' class='btn-group btn-group-xs -view edit search' role='group'></div>");
      var btnDrag = $("<button class='btn btn-default button-drag'><span class='glyphicon glyphicon-move'></span></button>");
      var btnRemove = $("<button class='btn btn-default button-delete'><span class='glyphicon glyphicon-remove'></span></button>");
      wrapper.append(btnDrag, btnRemove);
      template.on("view edit search", function (e) {
        if (e.type === "view") wrapper.hide();
        else wrapper.show();
        e.stopPropagation();
      });
      if (mode === "view") { wrapper.hide(); }

      btnRemove.click(function (e) {
        e.preventDefault();
        valTemplate.remove();
        individual.set( rel_uri, individual.get(rel_uri).filter(function (item) { return item.id !== value.id; }) );
        if ( value.is("v-s:Embedded") && value.hasValue("v-s:parent", individual) ) {
          value.delete();
        }
      }).mouseenter(function () {
        valTemplate.addClass("red-outline");
      }).mouseleave(function () {
        valTemplate.removeClass("red-outline");
      });

      //Sortable scroll bugfix
      btnDrag.mouseenter(function () {
        valTemplate.addClass("gray-outline");
      }).mouseleave(function () {
        valTemplate.removeClass("gray-outline");
      }).mousedown(function () {
        relContainer.addClass("sortable-overflow");
      }).mouseup(function () {
        relContainer.removeClass("sortable-overflow");
      });

      if (valTemplate.css("display") !== "inline") {
        wrapper.addClass("block");
      }
      if (valTemplate.css("display") === "table-row" || valTemplate.prop("tagName") === "TR") {
        var cell = valTemplate.children().last();
        cell.css("position", "relative").append(wrapper);
      } else {
        valTemplate.css("position", "relative");
        valTemplate.append(wrapper);
      }
    }
    return valTemplate;
  }

  // Property validation according to specification
  function validate(individual, property_uri, spec) {
    var result = {
      state: true,
      cause: []
    };
    if (!spec) { return result; }
    var values = individual.get(property_uri);
    // cardinality check
    if (spec.hasValue("v-ui:minCardinality")) {
      var minCardinalityState = (values.length >= spec["v-ui:minCardinality"][0] &&
      // filter empty values
      values.length === values.filter(function(item) {
        return (
          typeof item === "boolean" ? true :
          typeof item === "number" ? true : !!item
        ) ;
      }).length);
      result.state = result.state && minCardinalityState;
      if (!minCardinalityState) {
        result.cause.push("v-ui:minCardinality");
      }
    }
    if (spec.hasValue("v-ui:maxCardinality")) {
      var maxCardinalityState = (
        values.length <= spec["v-ui:maxCardinality"][0] &&
        // filter empty values
        values.length === values.filter(function(item) {
          return (
            typeof item === "boolean" ? true :
            typeof item === "number" ? true : !!item
          ) ;
        }).length
      );
      result.state = result.state && maxCardinalityState;
      if (!maxCardinalityState) {
        result.cause.push("v-ui:maxCardinality");
      }
    }
    // check each value
    result = result && values.reduce(function (result, value) {
      // regexp check
      if (spec.hasValue("v-ui:regexp")) {
        var regexp = new RegExp(spec["v-ui:regexp"][0]);
        var regexpState = regexp.test(value.toString());
        result.state = result.state && regexpState;
        if (!regexpState) {
          result.cause.push("v-ui:regexp");
        }
      }
      // range check
      switch (spec["rdf:type"][0].id) {
        case "v-ui:DatatypePropertySpecification" :
          if (spec.hasValue("v-ui:minValue")) {
            var minValueState = (value >= spec["v-ui:minValue"][0]);
            result.state = result.state && minValueState;
            if (!minValueState) {
              result.cause.push("v-ui:minValue");
            }
          }
          if (spec.hasValue("v-ui:maxValue")) {
            var maxValueState = (value <= spec["v-ui:maxValue"][0]);
            result.state = result.state && maxValueState;
            if (!maxValueState) {
              result.cause.push("v-ui:maxValue");
            }
          }
          if (spec.hasValue("v-ui:minLength")) {
            var minLengthState = (value.toString().length >= spec["v-ui:minLength"][0]);
            result.state = result.state && minLengthState;
            if (!minLengthState) {
              result.cause.push("v-ui:minLength");
            }
          }
          if (spec.hasValue("v-ui:maxLength")) {
            var maxLengthState = (value.toString().length <= spec["v-ui:maxLength"][0]);
            result.state = result.state && maxLengthState;
            if (!maxLengthState) {
              result.cause.push("v-ui:maxLength");
            }
          }
          break;
        case "v-ui:ObjectPropertySpecification" :
          break;
      }
      return result;
    }, result);
    return result;
  }

});
