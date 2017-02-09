// Veda controls implemented as JQuery plugins
"use strict";
;(function( $ ) {

  var defaultDelay = 750;

  // INPUT CONTROLS

  // Generic literal input behaviour
  var veda_literal_input = function( options ) {
    var opts = $.extend( {}, veda_literal_input.defaults, options ),
      control = $(opts.template),
      input = $(".form-control", control),
      spec = opts.spec,
      placeholder = spec && spec.hasValue("v-ui:placeholder") ? spec["v-ui:placeholder"].join(" ") : "",
      property_uri = opts.property_uri,
      individual = opts.individual,
      timeout;

    control.isSingle = opts.isSingle || (spec && spec.hasValue("v-ui:maxCardinality") ? spec["v-ui:maxCardinality"][0] === 1 : true);

    input.attr("placeholder", placeholder)
      .on("change focusout", changeHandler)
      .keyup( function (e) {
        if (!control.isSingle) { return; }
        if (timeout) { clearTimeout(timeout); }
        timeout = setTimeout(keyupHandler, defaultDelay, e);
      });

    individual.on("individual:propertyModified", modifiedHandler);
    control.one("remove", function () {
      individual.off("individual:propertyModified", modifiedHandler);
    });
    modifiedHandler(property_uri);

    function modifiedHandler (doc_property_uri) {
      if (doc_property_uri === property_uri && control.isSingle) {
        var field = input[0];
        var value = veda.Util.formatValue( individual[property_uri][0] );
        value = typeof value !== "undefined" ? value : "";
        try {
          var start = field.selectionStart;
          var end = field.selectionEnd;
          field.value = value;
          field.selectionStart = start;
          field.selectionEnd = end;
        } catch (ex) {
          field.value = value;
          console.log("selectionStart/End error:", property_uri, value, typeof value);
        }
      }
    }
    function changeHandler (e) {
      var value = opts.parser(this.value);
      if (control.isSingle) {
        individual[property_uri] = [value];
      } else {
        individual[property_uri] = individual[property_uri].concat(value);
      }
    }
    function keyupHandler (e) {
      var input = $(e.target);
      if (
        e.which !== 188
        && e.which !== 190
        && e.which !== 110
        && input.val() !== input.data("prev")
      ) {
        input.data("prev", input.val());
        input.change();
      }
    }

    this.on("veda_focus", function (e) {
      input.trigger("focus");
      e.stopPropagation();
    });

    this.on("view edit search", function (e) {
      e.stopPropagation();
    });

    this.val = function (value) {
      if (!value) return input.val();
      return input.val( veda.Util.formatValue(value) );
    }

    if (spec && spec.hasValue("v-ui:tooltip")) {
      control.tooltip({
        title: spec["v-ui:tooltip"].join(", "),
        placement: "bottom",
        container: control,
        trigger: "focus",
        animation: false
      });
    }

    return control;
  };
  veda_literal_input.defaults = {
    parser: function (input) {
      return (input || null);
    }
  };

  // Generic input
  $.fn.veda_generic = function( options ) {
    var opts = $.extend( {}, $.fn.veda_generic.defaults, options ),
      control = veda_literal_input.call(this, opts);

    this.append(control);
    return this;
  };
  $.fn.veda_generic.defaults = {
    template: $("#string-control-template").html(),
    parser: function (input) {
      if ( moment(input, ["DD.MM.YYYY HH:mm", "DD.MM.YYYY", "YYYY-MM-DD"], true).isValid() ) {
        return moment(input, "DD.MM.YYYY HH:mm").toDate();
      } else if ( !isNaN( parseFloat( input.split(" ").join("").split(",").join(".") ) ) ) {
        return parseFloat( input.split(" ").join("").split(",").join(".") );
      } else if ( !isNaN( parseInt( input.split(" ").join("").split(",").join("."), 10 ) ) ) {
        return parseInt( input.split(" ").join("").split(",").join("."), 10 );
      } else if ( input === "true" ) {
        return true;
      } else if ( input === "false" ) {
        return false;
      } else {
        var individ = new veda.IndividualModel(input);
        if ( individ.isSync() && !individ.isNew() ) { return ind; }
      }
      return (input ? new String(input) : null);
    }
  };

  // String input
  $.fn.veda_string = function( options ) {
    var opts = $.extend( {}, $.fn.veda_string.defaults, options ),
      control = veda_literal_input.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_string.defaults = {
    template: $("#string-control-template").html(),
    parser: function (input) {
      return (input ? new String(input) : null);
    },
    isSingle: true
  };

  // Text input
  $.fn.veda_text = function( options ) {
    var opts = $.extend( {}, $.fn.veda_text.defaults, options ),
      control = veda_literal_input.call(this, opts);
    var ta = $("textarea", control);
    ta.attr("rows", this.attr("rows"));
    autosize(ta);
    this.on("edit", function () {
      autosize.update(ta);
    });
    this.on("remove", function () {
      autosize.destroy(ta);
    });
    this.append(control);
    return this;
  };
  $.fn.veda_text.defaults = {
    template: $("#text-control-template").html(),
    parser: function (input) {
      return (input ? new String(input) : null);
    },
    isSingle: true
  };

  // Integer control
  $.fn.veda_integer = function( options ) {
    var opts = $.extend( {}, $.fn.veda_integer.defaults, options ),
      control = veda_literal_input.call(this, opts);
    this.on("view edit search", function (e) {
      e.stopPropagation();
      if (e.type === "search") {
        control.isSingle = false;
      }
    });
    this.append(control);
    return this;
  };
  $.fn.veda_integer.defaults = {
    template: $("#integer-control-template").html(),
    parser: function (input) {
      var int = parseInt( input.split(" ").join("").split(",").join("."), 10 );
      return !isNaN(int) ? int : null;
    }
  };

  // Decimal control
  $.fn.veda_decimal = function( options ) {
    var opts = $.extend( {}, $.fn.veda_decimal.defaults, options ),
      control = veda_literal_input.call(this, opts);
    this.on("view edit search", function (e) {
      e.stopPropagation();
      if (e.type === "search") {
        control.isSingle = false;
      }
    });
    this.append(control);
    return this;
  };
  $.fn.veda_decimal.defaults = {
    template: $("#decimal-control-template").html(),
    parser: function (input) {
      var float = parseFloat( input.split(" ").join("").split(",").join(".") );
      return !isNaN(float) ? float : null;
    }
  };

  // Datetime control
  var veda_dateTime = function (options) {
    var opts = $.extend( {}, veda_dateTime.defaults, options ),
      control = $(opts.template),
      format = opts.format,
      spec = opts.spec,
      placeholder = spec && spec.hasValue("v-ui:placeholder") ? spec["v-ui:placeholder"].join(" ") : "",
      property_uri = opts.property_uri,
      individual = opts.individual,
      isSingle = spec && spec.hasValue("v-ui:maxCardinality") ? spec["v-ui:maxCardinality"][0] === 1 : true,
      input = $("input", control),
      change;

    input.attr("placeholder", placeholder);

    var singleValueHandler = function (doc_property_uri, values) {
      if (doc_property_uri === property_uri) {
        if (values.length) {
          input.val( moment(values[0]).format(format) );
        } else {
          input.val("");
        }
      }
    }

    if (isSingle) {
      change = function (value) {
        individual[property_uri] = [value];
      };
      if (individual.hasValue(property_uri)) {
        input.val( moment(individual[property_uri][0]).format(format) );
      }
      individual.on("individual:propertyModified", singleValueHandler);
      control.one("remove", function () {
        individual.off("individual:propertyModified", singleValueHandler);
      });
    } else {
      change = function (value) {
        individual[property_uri] = individual[property_uri].concat(value);
        input.val("");
      }
    }

    if (spec && spec.hasValue("v-ui:tooltip")) {
      control.tooltip({
        title: spec["v-ui:tooltip"].join(", "),
        placement: "auto left",
        container: control,
        trigger: "focus",
        animation: false
      });
    }

    control.datetimepicker({
      locale: "ru",
      allowInputToggle: true,
      format: format,
      sideBySide: true,
      useCurrent: true
    });

    input.on("change focusout", function () {
      var value = opts.parser( this.value );
      change(value);
    });

    this.on("veda_focus", function (e) {
      input.trigger("focus");
      e.stopPropagation();
    });

    this.on("view edit search", function (e) {
      e.stopPropagation();
      if (e.type === "search") {
        change = function (value) {
          individual[property_uri] = individual[property_uri].concat(value);
          input.val("");
        }
      }
    });

    this.val = function (value) {
      if (!value) return input.val();
      return input.val(value);
    }

    this.on("remove", function () {
      control.data("DateTimePicker").destroy();
    });

    return control;
  };
  veda_dateTime.defaults = {
    template: $("#datetime-control-template").html(),
    parser: function (input) {
      if (input) {
        var timestamp = moment(input, "DD.MM.YYYY HH:mm").toDate();
        return new Date(timestamp);
      }
      return null;
    },
    format: "DD.MM.YYYY HH:mm"
  };

  // Date control
  $.fn.veda_date = function( options ) {
    var opts = $.extend( {}, $.fn.veda_date.defaults, options ),
      control = veda_dateTime.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_date.defaults = {
    template: $("#datetime-control-template").html(),
    parser: function (input) {
      if (input) {
        var timestamp = moment(input, "DD.MM.YYYY").toDate();
        return new Date(timestamp);
      }
      return null;
    },
    format: "DD.MM.YYYY"
  };

  // Time control
  $.fn.veda_time = function( options ) {
    var opts = $.extend( {}, $.fn.veda_time.defaults, options ),
      control = veda_dateTime.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_time.defaults = {
    template: $("#datetime-control-template").html(),
    parser: function (input) {
      if (input) {
        var timestamp = moment(input, "HH:mm").toDate();
        var result = new Date(timestamp);
        result.setFullYear(1970);
        result.setMonth(0);
        result.setDate(1);
        return result;
      }
      return null;
    },
    format: "HH:mm"
  };

  // Date-Time control
  $.fn.veda_dateTime = function( options ) {
    var opts = $.extend( {}, $.fn.veda_dateTime.defaults, options ),
      control = veda_dateTime.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_dateTime.defaults = {
    template: $("#datetime-control-template").html(),
    parser: function (input) {
      if (input) {
        var timestamp = moment(input, "DD.MM.YYYY HH:mm").toDate();
        return new Date(timestamp);
      }
      return null;
    },
    format: "DD.MM.YYYY HH:mm"
  };


  // MULTILINGUAL INPUT CONTROLS

  // Generic multilingual input behaviour
  var veda_multilingual = function( options ) {
    var opts = $.extend( {}, veda_multilingual.defaults, options ),
      control = $(opts.template),
      inputTemplate = control.children().remove(),
      individual = opts.individual,
      property_uri = opts.property_uri,
      spec = opts.spec,
      placeholder = spec && spec.hasValue("v-ui:placeholder") ? spec["v-ui:placeholder"].join(" ") : "",
      timeout;

    Object.keys(veda.user.language).map(function (language_name) {
      var localedInput = inputTemplate.clone();
      var value = individual[property_uri].filter(function (item) {
        // Set value language to default if undefined
        if ( !item.language ) { item.language = veda.user.defaultLanguage; }
        return item.language === language_name;
      })[0];
      localedInput.find(".language-tag").text(language_name);
      localedInput.find(".form-control").attr("lang", language_name).val(value);
      control.append( localedInput );
    });

    var input = control.find(".form-control");
    input.attr("placeholder", placeholder)
      .on("change focusout", function () {
        var value = opts.parser( this.value, this );
        change(value);
      })
      .keyup( function (e) {
        if (timeout) { clearTimeout(timeout); }
        timeout = setTimeout(keyupHandler, defaultDelay, e);
      });

    individual.on("individual:propertyModified", handler);
    control.one("remove", function () {
      individual.off("individual:propertyModified", handler);
    });

    function keyupHandler (e) {
      var input = $(e.target);
      if (
        e.which !== 188
        && e.which !== 190
        && e.which !== 110
        && input.val() !== input.data("prev")
      ) {
        input.data("prev", input.val());
        input.change();
      }
    }
    function change (value) {
      var filtered = individual[property_uri].filter(function (item) {
        if ( !item.language ) { item.language = veda.user.defaultLanguage; }
        return item.language !== value.language ;
      });
      individual[property_uri] = value.length ? filtered.concat(value) : filtered;
    }
    function handler (doc_property_uri, values) {
      if (doc_property_uri === property_uri) {
        input.each(function () {
          var lang = this.lang;
          var value = values.filter(function (item) {
            // Set string language to default if undefined
            if ( !item.language ) { item.language = veda.user.defaultLanguage; }
            return item.language === lang;
          })[0];
          value = typeof value !== "undefined" ? value : "";
          try {
            var start = this.selectionStart;
            var end = this.selectionEnd;
            this.value = value;
            this.selectionStart = start;
            this.selectionEnd = end;
          } catch (ex) {
            this.value = value;
            console.log("selectionStart/End error:", property_uri, value, typeof value);
          }
        });
      }
    }

    this.on("veda_focus", function (e, value) {
      input.each(function () {
        // Set string language to default if undefined
        if ( !value.language ) { value.language = veda.user.defaultLanguage; }
        if ( value.language === this.lang ) { $(this).trigger("focus"); }
      });
      e.stopPropagation();
    });

    this.on("view edit search", function (e) {
      e.stopPropagation();
    });

    this.val = function (value) {
      if (!value) {
        return parser( input.val() );
      }
      // Set string language to default if undefined
      if ( !value.language ) { value.language = veda.user.defaultLanguage; }
      input.each(function () {
        if (value.language === this.lang) {
          this.value = value.toString();
        }
      });
    }

    if (spec && spec.hasValue("v-ui:tooltip")) {
      control.tooltip({
        title: spec["v-ui:tooltip"].join(", "),
        placement: "bottom",
        container: control,
        trigger: "focus",
        animation: false
      });
    }

    return control;
  };
  veda_multilingual.defaults = {
    parser: function (input, el) {
      var value = new String(input);
      value.language = $(el).attr("lang") || undefined;
      return value;
    }
  };

  // Multilingual string control
  $.fn.veda_multilingualString = function (options) {
    var opts = $.extend( {}, $.fn.veda_multilingualString.defaults, options ),
        $this = $(this);
    init();
    /*veda.on("language:changed", init);
    $this.one("remove", function () {
      veda.off("language:changed", init);
    });*/
    function init() {
      $this.empty();
      $this.append( veda_multilingual.call($this, opts) );
    }
    return this;
  };
  $.fn.veda_multilingualString.defaults = {
    template: $("#multilingual-string-control-template").html(),
  };

  // Multilingual text control
  $.fn.veda_multilingualText = function (options) {
    var opts = $.extend( {}, $.fn.veda_multilingualText.defaults, options ),
      $this = $(this);
    init();
    /*veda.on("language:changed", init);
    $this.one("remove", function () {
      veda.off("language:changed", init);
    });*/
    function init() {
      $this.empty();
      var control = veda_multilingual.call($this, opts);
      var ta = $("textarea", control);
      ta.attr("rows", $this.attr("rows"));
      autosize(ta);
      $this.on("edit", function () {
        autosize.update(ta);
      });
      $this.on("remove", function () {
        autosize.destroy(ta);
      });
      $this.append(control);
    }
    return this;
  };
  $.fn.veda_multilingualText.defaults = {
    template: $("#multilingual-text-control-template").html(),
  };

  // BOOLEAN CONTROL
  $.fn.veda_boolean = function( options ) {
    var opts = $.extend( {}, $.fn.veda_boolean.defaults, options ),
      control = $( opts.template ),
      input = $("input", control),
      individual = opts.individual,
      property_uri = opts.property_uri,
      spec = opts.spec;

    function handler (doc_property_uri) {
      if (doc_property_uri === property_uri) {
        if (individual.hasValue(property_uri)) {
          if (individual[property_uri][0] == true) {
            input.prop("checked", true).prop("readonly", false).prop("indeterminate", false);
          } else {
            input.prop("checked", false).prop("readonly", false).prop("indeterminate", false);
          }
        } else {
          input.prop("readonly", true).prop("indeterminate", true);
        }
      }
    }
    handler(property_uri);

    individual.on("individual:propertyModified", handler);
    this.one("remove", function () {
      individual.off("individual:propertyModified", handler);
    });

    input.click( function () {
      if ( input.prop("readonly") ) {
        individual[property_uri] = [ false ];
      } else if ( !input.prop("checked") ) {
        individual[property_uri] = [];
      } else {
        individual[property_uri] = [ true ];
      }
    });

    if ( input.closest(".checkbox.disabled").length ) {
      input.attr("disabled", "disabled");
    }

    this.on("view edit search", function (e) {
      e.stopPropagation();
      if (e.type === "view") {
        input.attr("disabled", "disabled")
        control.parents("label").tooltip("destroy");
      } else {
        if ( input.closest(".checkbox.disabled").length ) {
          input.attr("disabled", "disabled");
        } else {
          input.removeAttr("disabled");
        }
        if (spec && spec.hasValue("v-ui:tooltip")) {
          control.parents("label").tooltip({
            title: spec["v-ui:tooltip"].join(", "),
            placement: "bottom",
            container: control,
            trigger: "hover",
            animation: false
          });
        }
      }
    });
    this.append(control);
    return this;
  };
  $.fn.veda_boolean.defaults = {
    template: $("#boolean-control-template").html(),
  };

  // SELECT CONTROL

  $.fn.veda_select = function (options) {
    var opts = $.extend( {}, $.fn.veda_select.defaults, options ),
      control = $(opts.template),
      individual = opts.individual,
      property_uri = opts.property_uri || opts.rel_uri,
      spec = opts.spec,
      isSingle = spec && spec.hasValue("v-ui:maxCardinality") ? spec["v-ui:maxCardinality"][0] === 1 : true,
      select = $("select", control),
      first_opt = $("option", control),
      rangeRestriction = spec && spec.hasValue("v-ui:rangeRestriction") ? spec["v-ui:rangeRestriction"][0] : undefined,
      range = rangeRestriction ? [ rangeRestriction ] : (new veda.IndividualModel(property_uri))["rdfs:range"],
      queryPrefix = spec && spec.hasValue("v-ui:queryPrefix") ? spec["v-ui:queryPrefix"][0] : range.map(function (item) {return "'rdf:type'==='" + item.id + "'"}).join(" && "),
      placeholder = spec && spec.hasValue("v-ui:placeholder") ? spec["v-ui:placeholder"].join(" ") : (new veda.IndividualModel("v-s:SelectValueBundle"))["rdfs:label"].join(" "),
      options;

    if (spec && spec.hasValue("v-ui:optionValue")) {
      options = spec["v-ui:optionValue"];
    } else if (queryPrefix) {
      queryPrefix = queryPrefix.replace(/{\s*([^{}]+)\s*}/g, function (match) { return eval(match); });
      var queryResult = query(veda.ticket, queryPrefix).result;
      if (queryResult.length) {
        var individuals = get_individuals(veda.ticket, queryResult);
        options = individuals.map(function (json) {
          return new veda.IndividualModel(json);
        });
      }
    } else {
      options = [];
    }

    function populate() {
      if (options.length) {
        select.empty();
        first_opt.text(placeholder).data("value", null).appendTo(select);
        options.map(function (value, index) {
          if (index >= 100) return;
          var opt = first_opt.clone().appendTo(select);
          opt.text( veda.Util.formatValue(value) ).data("value", value);
          if ( isSingle && individual.hasValue(property_uri, value) ) {
            opt.attr("selected", "true");
          }
        });
      }
    }

    populate();

    select.change(function () {
      if (isSingle) {
        individual[property_uri] = [ $("option:selected", select).data("value") ];
      } else {
        individual[property_uri] = individual[property_uri].concat( $("option:selected", select).data("value") );
      }
    });

    function handler(doc_property_uri) {
      if (doc_property_uri === property_uri) {
        populate();
      }
    }
    individual.on("individual:propertyModified", handler);
    this.one("remove", function () {
      individual.off("individual:propertyModified", handler);
    });

    this.on("view edit search", function (e) {
      e.stopPropagation();
    });

    this.val = function (value) {
      if (!value) return $("select", this).val();
      return $("select", this).val( veda.Util.formatValue(value) );
    }

    this.append(control);
    return this;
  };
  $.fn.veda_select.defaults = {
    template: $("#select-control-template").html(),
  }

  // CHECKBOX GROUP CONTROL

  $.fn.veda_checkbox = function (options) {
    var opts = $.extend( {}, $.fn.veda_checkbox.defaults, options ),
      control = $(opts.template),
      individual = opts.individual,
      property_uri = opts.property_uri || opts.rel_uri,
      parser = opts.parser,
      spec = opts.spec,
      holder = $(".checkbox", control),
      rangeRestriction = spec && spec.hasValue("v-ui:rangeRestriction") ? spec["v-ui:rangeRestriction"][0] : undefined,
      range = rangeRestriction ? [ rangeRestriction ] : (new veda.IndividualModel(property_uri))["rdfs:range"],
      queryPrefix = spec && spec.hasValue("v-ui:queryPrefix") ? spec["v-ui:queryPrefix"][0] : range.map(function (item) {return "'rdf:type'==='" + item.id + "'"}).join(" && "),
      options;

    if (spec && spec.hasValue("v-ui:optionValue")) {
      options = spec["v-ui:optionValue"];
    } else if (queryPrefix) {
      queryPrefix = queryPrefix.replace(/{\s*([^{}]+)\s*}/g, function (match) { return eval(match); });
      var queryResult = query(veda.ticket, queryPrefix).result;
      if (queryResult.length) {
        var individuals = get_individuals(veda.ticket, queryResult);
        options = individuals.map(function (json) {
          return new veda.IndividualModel(json);
        });
      }
    } else {
      options = [];
    }

    function populate() {
      control.empty();
      options.map(function (value) {
        var hld = holder.clone().appendTo(control);
        var lbl = $("label", hld).append( veda.Util.formatValue(value) );
        var chk = $("input", lbl).data("value", value);
        if ( individual.hasValue(property_uri, value) ) {
          chk.attr("checked", "true");
        }
        chk.change(function () {
          if ( chk.is(":checked") ) {
            individual[property_uri] = individual[property_uri].concat( chk.data("value") );
          } else {
            individual[property_uri] = individual[property_uri].filter( function (i) {
              return i.valueOf() !== chk.data("value").valueOf();
            });
          }
        });
      });
    }

    populate();

    function handler(doc_property_uri) {
      if (doc_property_uri === property_uri) {
        populate();
      }
    }
    individual.on("individual:propertyModified", handler);
    this.one("remove", function () {
      individual.off("individual:propertyModified", handler);
    });

    this.on("view edit search", function (e) {
      e.stopPropagation();
      if (e.type === "view") {
        $("div.checkbox", control).addClass("disabled");
        $("input", control).attr("disabled", "true");
      } else {
        $("div.checkbox", control).removeClass("disabled");
        $("input", control).removeAttr("disabled");
      }
    });

    this.val = function (value) {
      if (!value) return $("input", this).map(function () { return this.value });
      populate();
      return this;
    }
    this.append(control);
    return this;
  };
  $.fn.veda_checkbox.defaults = {
    template: $("#checkbox-control-template").html(),
  }

  // RADIO GROUP CONTROL

  $.fn.veda_radio = function (options) {
    var opts = $.extend( {}, $.fn.veda_radio.defaults, options ),
      control = $(opts.template),
      individual = opts.individual,
      property_uri = opts.property_uri || opts.rel_uri,
      parser = opts.parser,
      spec = opts.spec,
      holder = $(".radio", control),
      rangeRestriction = spec && spec.hasValue("v-ui:rangeRestriction") ? spec["v-ui:rangeRestriction"][0] : undefined,
      range = rangeRestriction ? [ rangeRestriction ] : (new veda.IndividualModel(property_uri))["rdfs:range"],
      queryPrefix = spec && spec.hasValue("v-ui:queryPrefix") ? spec["v-ui:queryPrefix"][0] : range.map(function (item) {return "'rdf:type'==='" + item.id + "'"}).join(" && "),
      options;

    if (spec && spec.hasValue("v-ui:optionValue")) {
      options = spec["v-ui:optionValue"];
    } else if (queryPrefix) {
      queryPrefix = queryPrefix.replace(/{\s*([^{}]+)\s*}/g, function (match) { return eval(match); });
      var queryResult = query(veda.ticket, queryPrefix).result;
      if (queryResult.length) {
        var individuals = get_individuals(veda.ticket, queryResult);
        options = individuals.map(function (json) {
          return new veda.IndividualModel(json);
        });
      }
    } else {
      options = [];
    }

    function populate() {
      control.empty();
      options.map(function (value) {
        var hld = holder.clone().appendTo(control);
        var lbl = $("label", hld).append( veda.Util.formatValue(value) );
        var rad = $("input", lbl).data("value", value);
        if ( individual.hasValue(property_uri, value) ) {
          rad.attr("checked", "true");
        }
        rad.change(function () {
          if ( rad.is(":checked") ) {
            individual[property_uri] = [ rad.data("value") ];
          } else {
            individual[property_uri] = individual[property_uri].filter( function (i) {
              return i.valueOf() !== rad.data("value").valueOf();
            });
          }
        });
      });
    }

    populate();

    function handler(doc_property_uri) {
      if (doc_property_uri === property_uri) {
        populate();
      }
    }
    individual.on("individual:propertyModified", handler);
    this.one("remove", function () {
      individual.off("individual:propertyModified", handler);
    });

    this.on("view edit search", function (e) {
      e.stopPropagation();
      if (e.type === "view") {
        $("div.radio", control).addClass("disabled");
        $("input", control).attr("disabled", "true");
      } else {
        $("div.radio", control).removeClass("disabled");
        $("input", control).removeAttr("disabled");
      }
    });

    this.val = function (value) {
      if (!value) return $("input", this).map(function () { return this.value });
      populate();
      return this;
    }
    this.append(control);
    return this;
  };
  $.fn.veda_radio.defaults = {
    template: $("#radio-control-template").html(),
  }

  // Numeration control
  $.fn.veda_numeration = function( options ) {
    var opts = $.extend( {}, $.fn.veda_numeration.defaults, options ),
      control = $(opts.template),
      spec = opts.spec,
      placeholder = spec && spec.hasValue("v-ui:placeholder") ? spec["v-ui:placeholder"].join(" ") : "",
      isSingle = spec && spec.hasValue("v-ui:maxCardinality") ? spec["v-ui:maxCardinality"][0] === 1 : true,
      property_uri = opts.property_uri,
      individual = opts.individual,
      input = $(".form-control", control),
      button = $(".get-numeration-value", control);

    input.attr("placeholder", placeholder);

    function singleValueHandler (doc_property_uri, values) {
      if (doc_property_uri === property_uri) {
        input.val( values[0] );
      }
    }

    var change = function (value) {
      individual[property_uri] = [value];
    };

    input.val(individual[property_uri][0]);
    individual.on("individual:propertyModified", singleValueHandler);
    control.one("remove", function () {
      individual.off("individual:propertyModified", singleValueHandler);
    });

    if (spec && spec.hasValue("v-ui:tooltip")) {
      control.tooltip({
        title: spec["v-ui:tooltip"].join(", "),
        placement: "bottom",
        container: control,
        trigger: "focus",
        animation: false
      });
    }

    input.on("change focusout", function () {
      var value = opts.parser( this.value, this );
      change(value);
    });

    this.on("view edit search", function (e) {
      e.stopPropagation();
    });

    this.val = function (value) {
      if (!value) return input.val();
      return input.val(value);
    }

    button.on("click", function () {
      var prop = new veda.IndividualModel(property_uri);
      if (prop['v-s:hasNumerationMapper']) {
        for (var mapkey in prop['v-s:hasNumerationMapper']) {
          var map = prop['v-s:hasNumerationMapper'][mapkey];
          if (map['v-s:numerationClass'][0].id == individual['rdf:type'][0].id) {
            var numertor = map['v-s:hasNumerationRule'][0];
              var scope = eval(numertor['v-s:numerationScope'][0].toString())(null, individual);
              var nextValue = eval(numertor['v-s:numerationGetNextValue'][0].toString())(null, scope);
              individual[property_uri] = [nextValue];
          }
        }
      }
    });

    this.append(control);
    return this;
  };
  $.fn.veda_numeration.defaults = {
    template: $("#numeration-control-template").html(),
    parser: function (input) {
      var int = parseInt( input.split(" ").join("").split(",").join("."), 10 );
      return !isNaN(int) ? "" + int : null;
    }
  };

  // SOURCE CODE CONTROL
  $.fn.veda_source = function (options) {
    var self = this,
        opts = $.extend( {}, $.fn.veda_source.defaults, options ),
        control = $(opts.template),
        individual = opts.individual,
        property_uri = opts.property_uri,
        fscreen = $("#full-screen", control),
        editorEl = control.get(0);

    opts.value = individual.hasValue(property_uri) ? individual[property_uri][0].toString() : "";
    opts.change = function (value) {
      individual[property_uri] = [value];
    }
    if (typeof self.attr('data-mode') !== "undefined") opts.sourceMode = self.attr('data-mode');
    if (property_uri === "v-s:script") opts.sourceMode = "javascript";
    if (property_uri === "v-ui:template") opts.sourceMode = "htmlmixed";
    var editor = CodeMirror(editorEl, {
      value: opts.value,
      mode: opts.sourceMode,
      lineWrapping: true,
      matchBrackets: true,
      autoCloseBrackets: true,
      matchTags: true,
      autoCloseTags: true,
      lineNumbers: true
    });
    setTimeout( function () {
      editor.refresh();
    }, 100);
    this.on("view edit search", function (e) {
      e.stopPropagation();
      editor.refresh();
      e.type === "view"   ? ( editor.setOption("readOnly", "nocursor") ) :
      e.type === "edit"   ? ( editor.setOption("readOnly", false) ) :
      e.type === "search" ? ( editor.setOption("readOnly", false) ) :
      true;
    });

    editor.on("change", function () {
      var value = opts.parser( editor.doc.getValue() );
      opts.change(value);
    });
    function handler(property_modified, values) {
      if (property_modified === property_uri) {
        var doc = editor.getDoc();
        var value = doc.getValue();
        if (!values.length || values[0].toString() !== value) {
          var cursor = doc.getCursor();
          doc.setValue( values.length ? values[0].toString() : "" );
          doc.setCursor(cursor);
        }
      }
    }
    individual.on("individual:propertyModified", handler );
    this.on("remove", function () {
      individual.off("individual:propertyModified", handler);
    });

    fscreen.click(function () {
      var body = $("body"),
          all = $("body > *:not(script)"),
          parent = control.parent(),
          wrapper = $("<div class='fs-wrapper'></div>"),
          cm = $(".CodeMirror", control);
      if ( !parent.hasClass("fs-wrapper") ) {
        control.wrap( wrapper );
        cm.addClass("CodeMirror-fs");
        parent = control.parent();
        parent.detach();
        all.hide();
        body.append(parent);
      } else {
        parent.detach();
        cm.removeClass("CodeMirror-fs");
        self.append(parent);
        control.unwrap();
        all.show();
      }
      control.toggleClass("fs");
      fscreen.toggleClass("glyphicon-resize-full glyphicon-resize-small");
      editor.refresh();
    });

    this.append(control);
    return this;
  }
  $.fn.veda_source.defaults = {
    value: "",
    template: $("#source-control-template").html(),
    mode: "javascript",
    parser: function (input) {
      return (input || null);
      //return new String(input);
    }
  };

  // FILE UPLOAD CONTROL
  function uploadFile(file, fileType, maxSize, success, progress) {
    if (maxSize && file.size > maxSize * 1024 * 1024) {
      return veda.trigger("danger", {description: "Файл слишком большой (> " + maxSize + " Mb)"});
    }
    var ext = file.name.match(/\.\w+$/); ext = ( ext ? ext[0] : ext );
    if (fileType && fileType.split(",").indexOf(ext) < 0) {
      return veda.trigger("danger", {description: "Тип файла не разрешен (" + fileType + ")"});
    }
    var url = "/files",
        xhr = new XMLHttpRequest(),
        d = new Date(),
        path = ["", d.getFullYear(), d.getMonth() + 1, d.getDate()].join("/"),
        uri = veda.Util.guid(),
        fd = new FormData();
    xhr.open("POST", url, true);
    xhr.upload.onprogress = progress;
    xhr.onreadystatechange = function() {
      if (xhr.readyState == 4 && xhr.status == 200) {
        success(file, path, uri);
      }
    };
    fd.append("file", file);
    fd.append("path", path);
    fd.append("uri", uri);
    xhr.send(fd);
  }
  $.fn.veda_file = function( options ) {
    if (window.FormData) {
      var opts = $.extend( {}, $.fn.veda_file.defaults, options ),
        control = $(opts.templateAJAX),
        spec = opts.spec,
        individual = opts.individual,
        rel_uri = opts.rel_uri,
        rangeRestriction = spec && spec.hasValue("v-ui:rangeRestriction") ? spec["v-ui:rangeRestriction"][0] : undefined,
        range = rangeRestriction ? [ rangeRestriction ] : (new veda.IndividualModel(rel_uri))["rdfs:range"],
        isSingle = spec && spec.hasValue("v-ui:maxCardinality") ? spec["v-ui:maxCardinality"][0] === 1 : true,
        acceptedFileType = spec && spec.hasValue("v-ui:acceptedFileType") ? spec["v-ui:acceptedFileType"][0].valueOf() : undefined,
        maxFileSize = spec && spec.hasValue("v-ui:maxFileSize") ? spec["v-ui:maxFileSize"][0] : undefined;

      var fileInput = $("#file", control);
      if (!isSingle) fileInput.attr("multiple", "multiple");
      var btn = $("#btn", control),
          indicatorPercentage = $(".indicator-percentage", control),
          indicatorSpinner = $(".indicator-spinner", control);
      btn.click(function (e) {
        fileInput.click();
      });
      var files = [], n;
      var uploaded = function (file, path, uri) {
        var f = new veda.IndividualModel();
        f["rdf:type"] = range;
        f["v-s:fileName"] = [ file.name ];
        f["rdfs:label"] = [ file.name ];
        f["v-s:fileSize"] = [ file.size ];
        f["v-s:fileUri"] = [ uri ];
        f["v-s:filePath"] = [ path ];
        f["v-s:parent"] = [ individual ]; // v-s:File is subClassOf v-s:Embedded
        f.save();
        files.push(f);
        if (files.length === n) {
          if (isSingle) {
            individual[rel_uri] = files;
          } else {
            individual[rel_uri] = individual[rel_uri].concat(files);
          }
        }
        indicatorSpinner.empty().hide();
        indicatorPercentage.empty().hide();
      };
      var progress = function (progressEvent) {
        if (progressEvent.lengthComputable) {
          var percentComplete = Math.round(progressEvent.loaded / progressEvent.total * 100);
          indicatorPercentage.text(percentComplete + "%").show();
        } else {
          indicatorSpinner.show();
        }
      };
      fileInput.change(function () {
        files = [];
        n = this.files.length;
        for (var i = 0, file; (file = this.files && this.files[i]); i++) {
          uploadFile(file, acceptedFileType, maxFileSize, uploaded, progress)
        }
      });
      this.on("view edit search", function (e) {
        e.stopPropagation();
      });
      this.append(control);
      return this;
    } else {
      var opts = $.extend( {}, $.fn.veda_file.defaults, options ),
        control = $(opts.templateIFRAME),
        fileInput = $("input#file", control),
        filePath = $("input#path", control),
        fileUri = $("input#uri", control),
        form = $("form", control),
        iframe = $("iframe", control),
        spec = opts.spec,
        individual = opts.individual,
        rel_uri = opts.rel_uri,
        isSingle = spec && spec.hasValue("v-ui:maxCardinality") ? spec["v-ui:maxCardinality"][0] === 1 : true,
        id = veda.Util.guid();
      form.attr("target", id);
      iframe.attr("id", id).attr("name", id).attr("src", "javascript:void(0);");
      fileInput.change(function () {
        if (this.value) {
          var d = new Date(),
          path = ["", d.getFullYear(), d.getMonth() + 1, d.getDate()].join("/"),
          uri = veda.Util.guid(),
          name = this.value.match(/([^\\\/]+)$/gi)[0];
          filePath.val(path);
          fileUri.val(uri);
          form.submit();
          iframe.one("load", function () {
            var f = new veda.IndividualModel();
            f["rdf:type"] = [ new veda.IndividualModel("v-s:File") ];
            f["v-s:fileName"] = [ name ];
            f["rdfs:label"] = [ name ];
            f["v-s:fileUri"] = [ uri ];
            f["v-s:filePath"] = [ path ];
            f.save();
            if (isSingle) {
              individual[rel_uri] = [f];
            } else {
              individual[rel_uri] = individual[rel_uri].concat(f);
            }
          });
        }
      });
      this.on("view edit search", function (e) {
        e.stopPropagation();
      });
      this.append(control);
      return this;
    }
  }
  $.fn.veda_file.defaults = {
    templateAJAX: $("#file-control-template-ajax").html(),
    templateIFRAME: $("#file-control-template-iframe").html()
  };

  // OBJECT PROPERTY CONTROL
  $.fn.veda_link = function( options ) {
    var opts = $.extend( {}, $.fn.veda_link.defaults, options ),
      control = $(opts.template),
      template = this.attr("data-template") || "{individual['rdfs:label'].join(', ')}",
      individual = opts.individual,
      spec = opts.spec,
      placeholder = spec && spec.hasValue("v-ui:placeholder") ? spec["v-ui:placeholder"].join(" ") : (new veda.IndividualModel("v-s:StartTypingBundle"))["rdfs:label"].join(" "),
      queryPrefix = spec && spec.hasValue("v-ui:queryPrefix") ? spec["v-ui:queryPrefix"][0] : undefined,
      sort = spec && spec.hasValue("v-ui:sort") ? spec["v-ui:sort"][0] : "'rdfs:label_ru' asc , 'rdfs:label_en' asc , 'rdfs:label' asc",
      rangeRestriction = spec && spec.hasValue("v-ui:rangeRestriction") ? spec["v-ui:rangeRestriction"][0] : undefined,
      root = spec && spec.hasValue("v-ui:treeRoot") ? spec["v-ui:treeRoot"] : undefined,
      inProperty = spec && spec.hasValue("v-ui:treeInProperty") ? spec["v-ui:treeInProperty"] : undefined,
      outProperty = spec && spec.hasValue("v-ui:treeOutProperty") ? spec["v-ui:treeOutProperty"] : undefined,
      allowedClass = spec && spec.hasValue("v-ui:treeAllowedClass") ? spec["v-ui:treeAllowedClass"] : undefined,
      selectableClass = spec && spec.hasValue("v-ui:treeSelectableClass") ? spec["v-ui:treeSelectableClass"] : undefined,
      selectableFilter = spec && spec.hasValue("v-ui:treeSelectableFilter") ? spec["v-ui:treeSelectableFilter"] : undefined,
      displayedProperty = spec && spec.hasValue("v-ui:treeDisplayedProperty") ? spec["v-ui:treeDisplayedProperty"] : [ new veda.IndividualModel("rdfs:label") ],
      rel_uri = opts.rel_uri,
      isSingle = spec && spec.hasValue("v-ui:maxCardinality") ? spec["v-ui:maxCardinality"][0] === 1 : true,
      create = $("#create", control),
      dropdown = $("#dropdown", control),
      fulltext = $("#fulltext", control),
      tree = $("#tree", control),
      fullsearch = $("#fullsearch", control);

    this.removeAttr("data-template");
    function renderTemplate (individual) {
      return template.replace(/{\s*([^{}]+)\s*}/g, function (match) { return eval(match); })
    }

    if (queryPrefix) {
      queryPrefix = queryPrefix.replace(/{\s*([^{}]+)\s*}/g, function (match) { return eval(match); });
    } else {
      var relRange = rangeRestriction ? [ rangeRestriction ] : (new veda.IndividualModel(rel_uri))["rdfs:range"];
      if ( relRange && relRange.length && (relRange.length > 1 || relRange[0].id !== "rdfs:Resource") ) {
        var types = relRange.map(function (i) { return "'rdf:type' == '" + i.id + "'";})
        queryPrefix = "(" + types.join(" || ") + ")";
      }
    }

    // Select value
    function select(selected) {
      selected = selected instanceof Array ? selected : [ selected ];
      if (isSingle) {
        individual[rel_uri] = [ selected[0] ];
      } else {
        var filtered = selected.filter( function (i) {
          return individual[rel_uri].indexOf(i) < 0;
        });
        individual[rel_uri] = individual[rel_uri].concat(filtered);
      }
    }

    function createValue() {
      var newVal = new veda.IndividualModel();
      newVal["rdf:type"] = rangeRestriction ? [ rangeRestriction ] : (new veda.IndividualModel(rel_uri))["rdfs:range"];
      return newVal;
    }

    // Create feature
    if ( this.hasClass("create") || this.hasClass("full") ) {
      var inModal = this.hasClass("create-modal");
      create.click( function () {
        var newVal = createValue();
        if ( inModal ) {
          var modal = $("#individual-modal-template").html();
          var $modal = $(modal);
          var ok = $("#ok", $modal).click(function () {
            select(newVal);
          });
          $modal.modal({"show": false});
          $("body").append($modal);
          $modal.modal("show");
          var cntr = $(".modal-body", $modal);
          newVal.one("individual:beforeReset", function () {
            $modal.modal("hide").remove();
          });
          newVal.one("individual:afterSave", function () {
            select(newVal);
            $modal.modal("hide").remove();
          });
          var tmpl = newVal["rdf:type"][0].template ? $( newVal["rdf:type"][0].template["v-ui:template"][0].toString() ) : undefined;
          tmpl.removeClass("container").addClass("container-fluid");
          $(".action", tmpl).remove();
          newVal.present(cntr, tmpl, "edit");
          var template = cntr.children("[resource]");
          template.on("internal-validated", function () {
            var validation = template.data("validation");
            validation.state ? ok.removeAttr("disabled") : ok.attr("disabled", "disabled");
          });
        } else {
          select(newVal);
        }
      });

      // Hide create button for single value relations if value exists
      if (isSingle) {
        var singleValueHandler = function (modified_rel_uri, values) {
          if (modified_rel_uri === rel_uri) {
            if (values.length) {
              create.hide();
            } else {
              create.show();
            }
          }
        };
        individual.on("individual:propertyModified", singleValueHandler);
        create.one("remove", function () {
          individual.off("individual:propertyModified", singleValueHandler);
        });
      }

    } else {
      create.remove();
    }

    // Tree feature
    if (
      (this.hasClass("tree") || this.hasClass("full"))
      && (root && (inProperty || outProperty))
    ) {
      individual.treeConfig = {
        root: root,
        inProperty: inProperty,
        outProperty: outProperty,
        allowedClass: allowedClass,
        selectableClass: selectableClass,
        selectableFilter: selectableFilter,
        displayedProperty: displayedProperty
      };
      var treeTmpl = new veda.IndividualModel("v-ui:TreeTemplate");
      var modal = $("#search-modal-template").html();
      tree.click(function () {
        var $modal = $(modal);
        var cntr = $(".modal-body", $modal);
        $modal.on('hidden.bs.modal', function (e) {
          $modal.remove();
        });
        $modal.modal();
        $("body").append($modal);
        individual.present(cntr, treeTmpl);

        $("#ok", $modal).click( function (e) {
          var selected = cntr.data("selected");
          select( selected.map(function (uri) {
            return new veda.IndividualModel(uri);
          }) );
        });
      });
    } else {
      tree.remove();
    }

    // Fulltext search feature
    if ( this.hasClass("fulltext") || this.hasClass("full") ) {

      fulltext.attr("placeholder", placeholder);

      var timeout;

      var dataSource = function (input, callback) {
        if (timeout) { clearTimeout(timeout); }
        timeout = setTimeout(mkQuery, input ? defaultDelay : 0, input, callback);
      }

      var mkQuery = function (input, callback) {
        var queryString;
        if ( input ) {
          var tokens = input.replace(/[-*]/g, " ").replace(/\s+/g, " ").trim().split(" ");
          var q = tokens.map(function (token) { return "'*' == '" + token + "*'" }).join(" && ");
          queryString = "(" + queryPrefix + ") && (" + q + ")" ;
        } else {
          queryString = queryPrefix;
        }
        var limit = opts.limit || 0,
            queryResult = query(veda.ticket, queryString, sort, null, null, limit, limit ).result,
            result = [],
            getList = queryResult.filter( function (uri, index) {
              return ( veda.cache[uri] ? (result.push(veda.cache[uri]), false) : true );
            }),
            individuals = getList.length ? get_individuals(veda.ticket, getList) : [];
        individuals.map( function (json) {
          result.push( new veda.IndividualModel(json) );
        });
        callback(result);
      }

      var typeAhead = fulltext.typeahead (
        {
          minLength: 3,
          highlight: true
        },
        {
          name: "dataset",
          source: dataSource,
          displayKey: function (individual) {
            var result;
            try {
              result = renderTemplate(individual);
            } catch (ex) {
              console.log(ex);
              result = individual.id;
            }
            return result === "" ? individual.id : result;
          }
        }
      );

      // Assign values in individual
      typeAhead.on("typeahead:selected", function (e, selected) {
        select(selected);
      });

      // Clear values from individual if isSingle && typeAhead was emptied
      typeAhead.change(function () {
        if (isSingle && this.value === "") {
          individual[rel_uri] = [];
        }
      });

      // Fill in value in fulltext field
      var handler = function (doc_rel_uri) {
        if (doc_rel_uri === rel_uri) {
          if (isSingle && individual.hasValue(rel_uri)) {
            try {
              typeAhead.typeahead( "val", renderTemplate( individual[rel_uri][0]) );
            } catch (e) {
              typeAhead.typeahead("val", "");
            }
          } else {
            typeAhead.typeahead("val", "");
          }
        }
      }
      individual.on("individual:propertyModified", handler);
      control.one("remove", function () {
        individual.off("individual:propertyModified", handler);
      });

      handler(rel_uri);

    } else {
      fulltext.remove();
    }

    // Dropdown feature
    if ( (this.hasClass("dropdown") || this.hasClass("full")) && queryPrefix ) {
      dropdown.click(function () {
        var minLength = typeAhead.data().ttTypeahead.minLength;
        typeAhead.data().ttTypeahead.minLength = 0;
        typeAhead.data().ttTypeahead.input.trigger("queryChanged", "");
        typeAhead.focus();
        typeAhead.data().ttTypeahead.minLength = minLength;
      });
    } else {
      dropdown.remove();
    }

    // Search modal feature
    if ( this.hasClass("fullsearch") || this.hasClass("full") ) {
      // Search modal
      var tmpl = $("#search-modal-template").html();
      fullsearch.on("click", function (e) {
        var $modal = $(tmpl);
        $modal.on('hidden.bs.modal', function (e) {
          $modal.remove();
        });
        $modal.modal();
        $("body").append($modal);
        var srch = new veda.SearchModel(undefined, $(".modal-body", $modal), queryPrefix, sort);
        // Add found values
        $("#ok", $modal).on("click", function (e) {
          $(this).off("click");
          var selected = [];
          for (var uri in srch.selected) {
            selected.push( srch.selected[uri] );
          }
          select(selected);
        });
      });
    } else {
      fullsearch.remove();
    }

    if ( !$("input", control).length ) {
      $(".input-group", control).toggleClass("input-group btn-group");
      $(".input-group-addon", control).toggleClass("input-group-addon btn-default btn-primary");
    }

    this.on("view edit search", function (e) {
      e.stopPropagation();
      if (e.type === "search") {
        isSingle = false;
      }
    });

    this.append(control);
    return this;
  };
  $.fn.veda_link.defaults = {
    template: $("#link-control-template").html(),
    limit: 50
  };

})( jQuery );
