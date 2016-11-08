// Veda controls implemented as JQuery plugins
;(function( $ ) { "use strict";

  // INPUT CONTROLS

  // Generic literal input behaviour
  var veda_literal_input = function( options ) {
    var opts = $.extend( {}, veda_literal_input.defaults, options ),
      control = $(opts.template),
      input = $(".form-control", control),
      spec = opts.spec,
      placeholder = spec && spec.hasValue("v-ui:placeholder") ? spec["v-ui:placeholder"][0] : input.attr("placeholder"),
      isSingle = opts.isSingle || (spec && spec.hasValue("v-ui:maxCardinality") ? spec["v-ui:maxCardinality"][0] == 1 : true),
      property_uri = opts.property_uri,
      individual = opts.individual;

    input.attr("placeholder", placeholder);

    function singleValueHandler (doc_property_uri, values) {
      if (doc_property_uri === property_uri) {
        input.val( values[0] );
      }
    }

    var change = function (value) {
      individual[property_uri] = individual[property_uri].concat(value);
      input.val("");
    }

    if (isSingle) {
      change = function (value) {
        individual[property_uri] = [value];
      };
      input.val(individual[property_uri][0]);
      individual.on("individual:propertyModified", singleValueHandler);
      control.one("remove", function () {
        individual.off("individual:propertyModified", singleValueHandler);
      });
    }

    if (spec && spec.hasValue("v-ui:tooltip")) {
      control.tooltip({
        title: spec["v-ui:tooltip"].join(", "),
        placement: "bottom",
        container: control,
        trigger: "focus"
      });
    }

    input.on("change focusout", function () {
      var value = opts.parser( this.value, this );
      change(value);
    });

    if (isSingle) {
      input.keyup( function (e) {
        individual.off("individual:propertyModified", singleValueHandler);
        if (e.which !== 188 && e.which !== 190 && e.which !== 110 ) {
          if (this.value !== $(this).data("previousValue")) {
            $(this).data("previousValue", this.value);
            $(this).change();
          }
        }
        individual.on("individual:propertyModified", singleValueHandler);
      });
    }

    this.on("veda_focus", function (e) {
      input.trigger("focus");
      e.stopPropagation();
    });

    this.on("view edit search", function (e) {
      e.stopPropagation();
      /*if (e.type === "search") {
        change = function (value) {
          individual[property_uri] = individual[property_uri].concat(value);
          input.val("");
        }
      }*/
    });

    this.val = function (value) {
      if (!value) return input.val();
      return input.val(value);
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
      } else if ( !isNaN( parseFloat( input.replace(",", ".") ) ) ) {
        return parseFloat( input.replace(",", ".") )
      } else if ( !isNaN( parseInt( input.replace(",", "."), 10 ) ) ) {
        return parseInt( input.replace(",", "."), 10 )
      } else if ( input === "true" ) {
        return true;
      } else if ( input === "false" ) {
        return false;
      } else {
        var ind = new veda.IndividualModel(input);
        if ( ind["rdf:type"][0].id !== "rdfs:Resource" ) { return ind; }
      }
      return (input || null);
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
      return (input || null);
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
    setTimeout (function () {
      autosize.update(ta);
    }, 100);
    this.on("remove", function () {
      autosize.destroy(ta);
    });
    this.append(control);
    return this;
  };
  $.fn.veda_text.defaults = {
    template: $("#text-control-template").html(),
    parser: function (input) {
      return (input || null);
    },
    isSingle: true
  };

  // Boolean text control
  $.fn.veda_boolean = function( options ) {
    var opts = $.extend( {}, $.fn.veda_boolean.defaults, options ),
      control = veda_literal_input.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_boolean.defaults = {
    template: $("#boolean-control-template").html(),
    parser: function (input) {
      return (input === "true");
    }
  };

  // Integer control
  $.fn.veda_integer = function( options ) {
    var opts = $.extend( {}, $.fn.veda_integer.defaults, options ),
      control = veda_literal_input.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_integer.defaults = {
    template: $("#integer-control-template").html(),
    parser: function (input) {
      var int = parseInt( input.replace(",", "."), 10 );
      return !isNaN(int) ? int : null;
    }
  };

  // Numeration control
  $.fn.veda_numeration = function( options ) {
    var opts = $.extend( {}, $.fn.veda_numeration.defaults, options ),
      control = $(opts.template),
      spec = opts.spec,
      placeholder = spec && spec.hasValue("v-ui:placeholder") ? spec["v-ui:placeholder"][0] : "",
      isSingle = spec && spec.hasValue("v-ui:maxCardinality") ? spec["v-ui:maxCardinality"][0] == 1 : true,
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
        trigger: "focus"
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
      var int = parseInt( input.replace(",", "."), 10 );
      return !isNaN(int) ? "" + int : null;
    }
  };

  // Decimal control
  $.fn.veda_decimal = function( options ) {
    var opts = $.extend( {}, $.fn.veda_decimal.defaults, options ),
      control = veda_literal_input.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_decimal.defaults = {
    template: $("#decimal-control-template").html(),
    parser: function (input) {
      var float = parseFloat( input.replace(",", ".") );
      return !isNaN(float) ? float : null;
    }
  };
/*
  // Datetime control
  $.fn.veda_dateTime = function (options) {
    var opts = $.extend( {}, $.fn.veda_dateTime.defaults, options ),
      control = veda_literal_input.call(this, opts),
      individual = opts.individual,
      property_uri = opts.property_uri,
      spec = opts.spec,
      isSingle = spec && spec.hasValue("v-ui:maxCardinality") ? spec["v-ui:maxCardinality"][0] == 1 : true,
      input = $("input", control);
    control.datetimepicker({
      locale: "ru",
      allowInputToggle: true,
      format: "DD.MM.YYYY HH:mm"
    });
    if (isSingle) input.val( moment(individual[property_uri][0]).format("DD.MM.YYYY HH:mm") );
    this.append(control);
    return this;
  };
  $.fn.veda_dateTime.defaults = {
    template: $("#datetime-control-template").html(),
    parser: function (input) {
      var timestamp = moment(input, "DD.MM.YYYY HH:mm").toDate();
      return new Date(timestamp);
    }
  };
*/

  $.fn.veda_dateTime = function (options) {
    var opts = $.extend( {}, $.fn.veda_dateTime.defaults, options ),
      control = $(opts.template),
      spec = opts.spec,
      placeholder = spec && spec.hasValue("v-ui:placeholder") ? spec["v-ui:placeholder"][0] : "",
      property_uri = opts.property_uri,
      individual = opts.individual,
      isSingle = spec && spec.hasValue("v-ui:maxCardinality") ? spec["v-ui:maxCardinality"][0] == 1 : true,
      input = $("input", control),
      change;

    input.attr("placeholder", placeholder);

    var singleValueHandler = function (doc_property_uri, values) {
      if (doc_property_uri === property_uri) {
        if (values.length) {
          input.val( moment(values[0]).format("DD.MM.YYYY HH:mm") );
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
        input.val( moment(individual[property_uri][0]).format("DD.MM.YYYY HH:mm") );
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
        placement: "top",
        container: control,
        trigger: "focus"
      });
    }

    control.datetimepicker({
      locale: "ru",
      allowInputToggle: true,
      format: "DD.MM.YYYY HH:mm",
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
    this.append(control);

    this.on("remove", function () {
      control.data("DateTimePicker").destroy();
    });

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
    }
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
      placeholder = spec && spec.hasValue("v-ui:placeholder") ? spec["v-ui:placeholder"][0] : "";

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
    input.attr("placeholder", placeholder);

    var change = function (value) {
      var filtered = individual[property_uri].filter(function (item) {
        if ( !item.language ) { item.language = veda.user.defaultLanguage; }
        return item.language !== value.language ;
      });
      individual[property_uri] = value.length ? filtered.concat(value) : filtered;
    }

    input.on("change focusout", function () {
      var value = opts.parser( this.value, this );
      change(value);
    });

    function handler (doc_property_uri, values) {
      if (doc_property_uri === property_uri) {
        input.each(function () {
          var lang = this.lang;
          var value = values.filter(function (item) {
            // Set string language to default if undefined
            if ( !item.language ) { item.language = veda.user.defaultLanguage; }
            return item.language === lang;
          })[0];
          this.value = value || "";
        });
      }
    }

    individual.on("individual:propertyModified", handler);
    control.one("remove", function () {
      individual.off("individual:propertyModified", handler);
    });

    input.keyup( function (e) {
      individual.off("individual:propertyModified", handler);
      if (e.which !== 188 && e.which !== 190 && e.which !== 110 ) {
        if (this.value !== $(this).data("previousValue")) {
          $(this).data("previousValue", this.value);
          $(this).change();
        }
      }
      individual.on("individual:propertyModified", handler);
    });

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
        trigger: "focus"
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
    veda.on("language:changed", init);
    $this.one("remove", function () {
      veda.off("language:changed", init);
    });
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
    veda.on("language:changed", init);
    $this.one("remove", function () {
      veda.off("language:changed", init);
    });
    function init() {
      $this.empty();
      var control = veda_multilingual.call($this, opts);
      var ta = $("textarea", control);
      ta.attr("rows", $this.attr("rows"));
      autosize(ta);
      setTimeout (function () {
        autosize.update(ta);
      }, 100);
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

  // BOOLEAN CHECKBOX CONTROL
  $.fn.veda_booleanCheckbox = function( options ) {
    var opts = $.extend( {}, $.fn.veda_booleanCheckbox.defaults, options ),
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
            trigger: "hover"
          });
        }
      }
    });
    this.append(control);
    return this;
  };
  $.fn.veda_booleanCheckbox.defaults = {
    template: $("#boolean-checkbox-control-template").html(),
  };

  // LITERAL SELECT CONTROLS

  // Generic literal select behaviour
  var veda_selectLiteral = function (options) {
    var opts = $.extend( {}, veda_selectLiteral.defaults, options ),
      control = $(opts.template),
      individual = opts.individual,
      property_uri = opts.property_uri,
      parser = opts.parser,
      spec = opts.spec,
      isSingle = spec && spec.hasValue("v-ui:maxCardinality") ? spec["v-ui:maxCardinality"][0] == 1 : true,
      optionProperty = opts.optionProperty,
      select = $("select", control),
      first_opt = $("option", control);

    function populate() {
      if (spec && spec.hasValue(optionProperty)) {
        select.empty().append(first_opt);
        spec[optionProperty].map(function (value) {
          var opt = first_opt.clone().text(value).val(value).appendTo(select);
          if (isSingle && individual.hasValue(property_uri) && individual[property_uri][0].toString() === value.toString()) {
            opt.attr("selected", "true");
          }
        });
      }
    }

    populate();

    if (isSingle) {
      select.change(function () {
        individual[property_uri] = [ parser(select.val()) ];
      });
    } else {
      select.change(function () {
        individual[property_uri] = individual[property_uri].concat( parser(select.val()) );
      });
    }

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
      return $("select", this).val( value.toString() );
    }
    return control;
  };
  veda_selectLiteral.defaults = {
    template: $("#select-control-template").html(),
  }

  // Integer select control
  $.fn.veda_integerSelect = function (options) {
    var opts = $.extend( {}, $.fn.veda_integerSelect.defaults, options ),
      control = veda_selectLiteral.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_integerSelect.defaults = {
    optionProperty: "v-ui:optionIntegerValue",
    parser: function (input) {
      var int = parseInt(input, 10);
      return !isNaN(int) ? int : null;
    }
  };
  // Decimal select control
  $.fn.veda_decimalSelect = function (options) {
    var opts = $.extend( {}, $.fn.veda_decimalSelect.defaults, options ),
      control = veda_selectLiteral.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_decimalSelect.defaults = {
    optionProperty: "v-ui:optionDecimalValue",
    parser: function (input) {
      var float = parseFloat(input.replace(",", "."));
      return !isNaN(float) ? float : null;
    }
  };
  // String select control
  $.fn.veda_stringSelect = function (options) {
    var opts = $.extend( {}, $.fn.veda_stringSelect.defaults, options ),
      control = veda_selectLiteral.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_stringSelect.defaults = {
    optionProperty: "v-ui:optionStringValue",
    parser: function (input, el) {
      return (input || null);
    }
  };
  // Datetime select control
  $.fn.veda_datetimeSelect = function (options) {
    var opts = $.extend( {}, $.fn.veda_datetimeSelect.defaults, options ),
      control = veda_selectLiteral.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_datetimeSelect.defaults = {
    optionProperty: "v-ui:optionDatetimeValue",
    parser: function (input) {
      var timestamp = Date.parse(input);
      return !isNaN(timestamp) ? new Date(timestamp) : null;
    }
  };

  // CHECKBOX GROUP CONTROL

  // Generic checkbox group behaviour

  var veda_checkbox = function (options) {
    var opts = $.extend( {}, veda_checkbox.defaults, options ),
      control = $(opts.template),
      individual = opts.individual,
      property_uri = opts.property_uri,
      parser = opts.parser,
      spec = opts.spec,
      optionProperty = opts.optionProperty,
      holder = $(".checkbox", control);

    function populate() {
      if (spec && spec.hasValue(optionProperty)) {
        control.empty();
        spec[optionProperty].map(function (value) {
          var hld = holder.clone().appendTo(control);
          var lbl = $("label", hld).append(value);
          var chk = $("input", lbl).val(value);
          var test = individual.hasValue(property_uri) && individual[property_uri].filter( function (i) { return i.toString() === value.toString() }).length;
          if (test) {
            chk.attr("checked", "true");
          }
          chk.change(function () {
            if ( chk.is(":checked") ) {
              individual[property_uri] = individual[property_uri].concat( parser( chk.val() ) );
            } else {
              individual[property_uri] = individual[property_uri].filter( function (i) {
                return i.toString() !== chk.val();
              });
            }
          })
        });
      }
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
    return control;
  };
  veda_checkbox.defaults = {
    template: $("#checkbox-control-template").html(),
  }

  // Integer checkbox group control
  $.fn.veda_integerCheckbox = function (options) {
    var opts = $.extend( {}, $.fn.veda_integerCheckbox.defaults, options ),
      control = veda_checkbox.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_integerCheckbox.defaults = {
    optionProperty: "v-ui:optionIntegerValue",
    parser: function (input) {
      var int = parseInt(input, 10);
      return !isNaN(int) ? int : null;
    }
  };
  // Decimal checkbox group control
  $.fn.veda_decimalCheckbox = function (options) {
    var opts = $.extend( {}, $.fn.veda_decimalCheckbox.defaults, options ),
      control = veda_checkbox.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_decimalCheckbox.defaults = {
    optionProperty: "v-ui:optionDecimalValue",
    parser: function (input) {
      var float = parseFloat(input.replace(",", "."));
      return !isNaN(float) ? float : null;
    }
  };
  // String checkbox group control
  $.fn.veda_stringCheckbox = function (options) {
    var opts = $.extend( {}, $.fn.veda_stringCheckbox.defaults, options ),
      control = veda_checkbox.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_stringCheckbox.defaults = {
    optionProperty: "v-ui:optionStringValue",
    parser: function (input, el) {
      return (input || null);
    }
  };
  // Detetime checkbox group control
  $.fn.veda_datetimeCheckbox = function (options) {
    var opts = $.extend( {}, $.fn.veda_datetimeCheckbox.defaults, options ),
      control = veda_checkbox.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_datetimeCheckbox.defaults = {
    optionProperty: "v-ui:optionDatetimeValue",
    parser: function (input) {
      var timestamp = Date.parse(input);
      return !isNaN(timestamp) ? new Date(timestamp) : null;
    }
  };

  // RADIO GROUP CONTROL

  // Generic radio group behaviour

  var veda_radio = function (options) {
    var opts = $.extend( {}, veda_radio.defaults, options ),
      control = $(opts.template),
      individual = opts.individual,
      property_uri = opts.property_uri,
      parser = opts.parser,
      spec = opts.spec,
      optionProperty = opts.optionProperty,
      holder = $(".radio", control).attr("name", individual.id + "_" + property_uri);

    function populate() {
      if (spec && spec.hasValue(optionProperty)) {
        control.empty();
        spec[optionProperty].map(function (value) {
          var hld = holder.clone().appendTo(control);
          var lbl = $("label", hld).append(value);
          var chk = $("input", lbl).val(value);
          var test = individual.hasValue(property_uri) && individual[property_uri].filter( function (i) { return i.toString() === value.toString() }).length;
          if (test) {
            chk.attr("checked", "true");
          }
          chk.change(function () {
            if ( chk.is(":checked") ) {
              individual[property_uri] = [ parser( chk.val() ) ];
            } else {
              individual[property_uri] = individual[property_uri].filter( function (i) {
                return i.toString() !== chk.val();
              });
            }
          })
        });
      }
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
    return control;
  };
  veda_radio.defaults = {
    template: $("#radio-control-template").html(),
  }

  // Integer radio group control
  $.fn.veda_integerRadio = function (options) {
    var opts = $.extend( {}, $.fn.veda_integerRadio.defaults, options ),
      control = veda_radio.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_integerRadio.defaults = {
    optionProperty: "v-ui:optionIntegerValue",
    parser: function (input) {
      var int = parseInt(input, 10);
      return !isNaN(int) ? int : null;
    }
  };
  // Decimal radio group control
  $.fn.veda_decimalRadio = function (options) {
    var opts = $.extend( {}, $.fn.veda_decimalRadio.defaults, options ),
      control = veda_radio.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_decimalRadio.defaults = {
    optionProperty: "v-ui:optionDecimalValue",
    parser: function (input) {
      var float = parseFloat(input.replace(",", "."));
      return !isNaN(float) ? float : null;
    }
  };
  // String radio group control
  $.fn.veda_stringRadio = function (options) {
    var opts = $.extend( {}, $.fn.veda_stringRadio.defaults, options ),
      control = veda_radio.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_stringRadio.defaults = {
    optionProperty: "v-ui:optionStringValue",
    parser: function (input, el) {
      return (input || null);
    }
  };
  // Detetime radio group control
  $.fn.veda_datetimeRadio = function (options) {
    var opts = $.extend( {}, $.fn.veda_datetimeRadio.defaults, options ),
      control = veda_radio.call(this, opts);
    this.append(control);
    return this;
  };
  $.fn.veda_datetimeRadio.defaults = {
    optionProperty: "v-ui:optionDatetimeValue",
    parser: function (input) {
      var timestamp = Date.parse(input);
      return !isNaN(timestamp) ? new Date(timestamp) : null;
    }
  };

  // SOURCE CODE CONTROL
  // supports only one-way binding (editor -> individual) except initial value
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

    fscreen.click(function () {
      var body = $("body"),
          all = $("body > *"),
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
  function uploadFile(file, success, progress) {
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
        isSingle = spec && spec.hasValue("v-ui:maxCardinality") ? spec["v-ui:maxCardinality"][0] == 1 : true;
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
          //indicatorPercentage.text("(" + percentComplete + "%)").show();
          indicatorPercentage.text(percentComplete + "%").show();
        } else {
          indicatorSpinner.show();
        }
      };
      fileInput.change(function () {
        files = [];
        n = this.files.length;
        for (var i = 0, file; (file = this.files && this.files[i]); i++) {
          uploadFile(file, uploaded, progress);
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
        isSingle = spec && spec.hasValue("v-ui:maxCardinality") ? spec["v-ui:maxCardinality"][0] == 1 : true,
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
      placeholder = spec && spec.hasValue("v-ui:placeholder") ? spec["v-ui:placeholder"][0] : "",
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
      isSingle = spec && spec.hasValue("v-ui:maxCardinality") ? spec["v-ui:maxCardinality"][0] == 1 : true,
      create = $("#create", control),
      dropdown = $("#dropdown", control),
      fulltext = $("#fulltext", control),
      tree = $("#tree", control),
      fullsearch = $("#fullsearch", control);

    if (queryPrefix) {
      queryPrefix = queryPrefix.replace(/{\s*([^{}]+)\s*}/g, function (match) { return eval(match); });
    } else {
      var relRange = (new veda.IndividualModel(rel_uri))["rdfs:range"];
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
      var container = this.parent();
      var inModal = this.hasClass("create-modal");
      create.click( function () {
        var newVal = createValue();
        if ( inModal ) {
          var modal = $("#individual-modal-template").html();
          var $modal = $(modal);
          var ok = $("#ok", $modal).click(function () {
            select(newVal);
          });
          $modal.modal();
          container.append($modal);
          var cntr = $(".modal-body", $modal);
          cntr.on("valid", function () {
            ok.removeAttr("disabled");
          });
          cntr.on("invalid", function () {
            ok.attr("disabled", "disabled");
          });
          newVal.one("individual:beforeReset", function () {
            $modal.modal("hide");
          });
          newVal.one("individual:afterSave", function () {
            select(newVal);
            $modal.modal("hide");
          });
          newVal.present(cntr, undefined, "edit");
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

      var typeAhead = fulltext.typeahead (
        {
          minLength: 3,
          highlight: true
        },
        {
          name: "dataset",
          source: function (input, cb) {
            var queryString;
            if ( input ) {
              var tokens = input.trim().replace("*", "").split(" ");
              var q = tokens.map(function (token) { return "'*' == '" + token + "*'" }).join(" && ");
              queryString = "(" + queryPrefix + ") && (" + q + ")" ;
            } else {
              queryString = queryPrefix;
            }
            var limit = opts.limit || 0,
                queryResult = query(veda.ticket, queryString, sort, null, null, limit, limit ),
                cached = [],
                result = [],
                getList = queryResult.filter( function (uri, index) {
                  return ( veda.cache[uri] ? (result.push(veda.cache[uri]), false) : true );
                }),
                individuals = getList.length ? get_individuals(veda.ticket, getList) : [];
            individuals.map( function (json) {
              result.push( new veda.IndividualModel(json) );
            });
            cb(result);
          },
          displayKey: function (individual) {
            var result;
            try {
              result = template.replace(/{\s*([^{}]+)\s*}/g, function (match) { return eval(match); });
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
      var handler = function (doc_rel_uri, values) {
        if (doc_rel_uri === rel_uri) {
          if (isSingle) {
            if (values.length) {
              var individual = values[0];
              try {
                typeAhead.typeahead("val", template.replace(/{\s*([^{}]+)\s*}/g, function (match) { return eval(match); }) );
              } catch (e) {
                typeAhead.typeahead("val", "");
              }
            } else {
              typeAhead.typeahead("val", "" );
            }
          } else {
            typeAhead.typeahead("val", "" );
          }
        }
      }

      individual.on("individual:propertyModified", handler);
      control.one("remove", function () {
        individual.off("individual:propertyModified", handler);
      });

      if (individual.hasValue(rel_uri)) {
        handler(rel_uri, individual[rel_uri]);
      }

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
    });

    this.append(control);
    return this;
  };
  $.fn.veda_link.defaults = {
    template: $("#link-control-template").html(),
    limit: 100
  };

  // OBJECT CHECKBOX GROUP CONTROL
  $.fn.veda_objectCheckbox = function (options) {
    var opts = $.extend( {}, $.fn.veda_objectCheckbox.defaults, options ),
      control = $(opts.template),
      individual = opts.individual,
      rel_uri = opts.rel_uri,
      parser = opts.parser,
      spec = opts.spec,
      optionProperty = opts.optionProperty,
      holder = $(".checkbox", control),
      template = new veda.IndividualModel( this.attr("data-template") || "v-ui:LabelTemplate" );

    function populate() {
      if (spec && spec.hasValue(optionProperty)) {
        control.empty();
        spec[optionProperty].map(function (value) {
          var hld = holder.clone().appendTo(control);
          var lbl = $("label", hld);
          var cont = $("<span>").appendTo(lbl);
          value.present(cont, template);
          var chk = $("input", lbl).val(value.id);
          var test = individual.hasValue(rel_uri) && individual[rel_uri].filter( function (i) { return i.id === value.id }).length;
          if (test) {
            chk.attr("checked", "true");
          }
          chk.change(function () {
            if ( chk.is(":checked") ) {
              individual[rel_uri] = individual[rel_uri].concat( parser( chk.val() ) );
            } else {
              individual[rel_uri] = individual[rel_uri].filter( function (i) {
                return i.id !== chk.val();
              });
            }
          })
        });
      }
    }

    populate();

    /*function handler(doc_rel_uri) {
      if (doc_rel_uri === rel_uri) {
        populate();
      }
    }
    individual.on("individual:propertyModified", handler);
    this.one("remove", function () {
      individual.off("individual:propertyModified", handler);
    });*/

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
      if (!value) return $("input", this).map(function () {
        if (this.value) return new veda.IndividualModel(this.value);
      });
      populate();
      return this;
    }

    this.append(control);
    return this;
  };
  $.fn.veda_objectCheckbox.defaults = {
    template: $("#checkbox-control-template").html(),
    optionProperty: "v-ui:optionObjectValue",
    parser: function (value) {
      return new veda.IndividualModel(value);
    }
  }

  // OBJECT RADIO GROUP CONTROL
  $.fn.veda_objectRadio = function (options) {
    var opts = $.extend( {}, $.fn.veda_objectRadio.defaults, options ),
      control = $(opts.template),
      individual = opts.individual,
      rel_uri = opts.rel_uri,
      parser = opts.parser,
      spec = opts.spec,
      optionProperty = opts.optionProperty,
      holder = $(".radio", control),
      template = new veda.IndividualModel( this.attr("data-template") || "v-ui:LabelTemplate" );

    function populate() {
      if (spec && spec.hasValue(optionProperty)) {
        control.empty();
        spec[optionProperty].map(function (value) {
          var hld = holder.clone().appendTo(control);
          var lbl = $("label", hld);
          var cont = $("<span>").appendTo(lbl);
          //setTimeout (function () {
            value.present(cont, template, "view");
          //}, 0);
          var chk = $("input", lbl).val(value.id);
          var test = individual.hasValue(rel_uri) && individual[rel_uri].filter( function (i) { return i.id === value.id }).length;
          if (test) {
            chk.attr("checked", "true");
          }
          chk.change(function () {
            if ( chk.is(":checked") ) {
              individual[rel_uri] = [ parser( chk.val() ) ];
            } else {
              individual[rel_uri] = individual[rel_uri].filter( function (i) {
                return i.id !== chk.val();
              });
            }
          })
        });
      }
    }

    populate();

    function handler(doc_rel_uri) {
      if (doc_rel_uri === rel_uri) {
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
      if (!value) return $("input", this).map(function () {
        if (this.value) return new veda.IndividualModel(this.value);
      });
      populate();
      return this;
    }

    this.append(control);
    return this;
  };
  $.fn.veda_objectRadio.defaults = {
    template: $("#radio-control-template").html(),
    optionProperty: "v-ui:optionObjectValue",
    parser: function (value) {
      return new veda.IndividualModel(value);
    }
  }

  // OBJECT SELECT CONTROL
  $.fn.veda_objectSelect = function (options) {
    var opts = $.extend( {}, $.fn.veda_objectSelect.defaults, options ),
      control = $(opts.template),
      individual = opts.individual,
      rel_uri = opts.rel_uri,
      parser = opts.parser,
      spec = opts.spec,
      isSingle = spec && spec.hasValue("v-ui:maxCardinality") ? spec["v-ui:maxCardinality"][0] == 1 : true,
      isObligatory = spec && spec.hasValue("v-ui:minCardinality") && spec["v-ui:minCardinality"][0] == 1,
      optionProperty = opts.optionProperty,
      select = $("select", control),
        first_opt = $("option", control),
      template = new veda.IndividualModel( this.attr("data-template") || "v-ui:LabelTemplate" );

    function populate() {
      if (spec && spec.hasValue(optionProperty)) {
        select.empty();
        if (!isObligatory) { select.append(first_opt); }
        spec[optionProperty].map(function (value) {
          var opt = first_opt.clone().empty().val(value.id).appendTo(select);
          value.present(opt, template, "view");
          if (isSingle && individual.hasValue(rel_uri) && individual[rel_uri][0].id === value.id) {
            opt.attr("selected", "true");
          }
        });
      }
    }

    populate();

    if (isSingle) {
      select.change(function () {
        var value = select.val();
        if (value) individual[rel_uri] = [ parser( select.val() ) ];
      });
    } else {
      select.change(function () {
        var value = select.val();
        var parsed = parser(value);
        if ( individual[rel_uri].indexOf(parsed) < 0 ) {
          individual[rel_uri] = individual[rel_uri].concat( parsed );
        }
      });
    }

    function handler(doc_rel_uri) {
      if (doc_rel_uri === rel_uri) {
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
      populate();
      return this;
    }

    this.append(control);
    return this;
  };
  $.fn.veda_objectSelect.defaults = {
    template: $("#select-control-template").html(),
    optionProperty: "v-ui:optionObjectValue",
    parser: function (value) {
      return new veda.IndividualModel(value);
    }
  }

})( jQuery );
