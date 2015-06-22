// Veda controls implemented as JQuery plugins
;(function( $ ) { "use strict";

	// INPUT CONTROLS

	// Generic literal input behaviour
	var veda_literal_input = function( options ) {
		var opts = $.extend( {}, veda_literal_input.defaults, options ),
			control = $(opts.template),
			spec = opts.spec,
			property_uri = opts.property_uri,
			individual = opts.individual,
			inputEl = $("[bound]", control);
	
		var singleValueHandler = function (doc_property_uri, values) {
			if (doc_property_uri === property_uri) {
				inputEl.val( values[0] );
			}
		}
		
		var change = function (value) { 
			individual[property_uri] = individual[property_uri].concat(value);
			inputEl.val("");
		}
		
		if (spec) {
			if (spec.hasValue("v-ui:maxCardinality") && spec["v-ui:maxCardinality"][0] == 1) {
				change = function (value) {
					individual[property_uri] = [value];
				};
				inputEl.val(individual[property_uri][0]);
				individual.on("individual:propertyModified", singleValueHandler);
				control.one("remove", function () {
					individual.off("individual:propertyModified", singleValueHandler);
				});
			}
			
			if (spec.hasValue("v-ui:tooltip")) {
				control.tooltip({
					title: spec["v-ui:tooltip"].join(", "),
					placement: "bottom",
					container: control,
					trigger: "focus"
				});
			}
		}

		inputEl.on("change", function ( e ) {
			var value = opts.parser( this.value, this );
			change(value);
		});
		
		this.on("view edit search", function (e) {
			e.stopPropagation();
		});
		
		this.val = function (value) {
			if (!value) return inputEl.val();
			return inputEl.val(value);
		}
		return control;
	};
	veda_literal_input.defaults = {
		parser: function (input) { return input; },
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
		parser: function (input, el) {
			var value = new String(input);
			return value != "" ? value : null;
		}
	};

	// Text input
	$.fn.veda_text = function( options ) {
		var opts = $.extend( {}, $.fn.veda_text.defaults, options ),
			control = veda_literal_input.call(this, opts);
	
		this.append(control);
		return this;
	};
	$.fn.veda_text.defaults = {
		template: $("#text-control-template").html(),
		parser: function (input, el) {
			var value = new String(input);
			return value != "" ? value : null;
		}
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
			return new Boolean(input == "true" ? true : false);
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
			var int = parseInt(input, 10);
			return !isNaN(int) ? new Number(int) : null;
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
			var float = parseFloat(input.replace(",", "."));
			return !isNaN(float) ? new Number(float) : null;
		}
	};

	// Datetime control
	$.fn.veda_dateTime = function (options) {
		var opts = $.extend( {}, $.fn.veda_dateTime.defaults, options ),
			control = veda_literal_input.call(this, opts);
		this.append(control);
		return this;
	};
	$.fn.veda_dateTime.defaults = {
		template: $("#datetime-control-template").html(),
		parser: function (input) {
			var timestamp = Date.parse(input);
			return !isNaN(timestamp) ? new Date(timestamp) : null;
		}
	};

	// MULTILINGUAL INPUT CONTROLS
	
	// Generic multilingual input behaviour
	var veda_multilingual = function( options ) {
		var opts = $.extend( {}, veda_multilingual.defaults, options ),
			control = veda_literal_input.call(this, opts),
			individual = opts.individual,
			property_uri = opts.property_uri,
			spec = opts.spec,
			isSingle = spec && spec.hasValue("v-ui:maxCardinality") && spec["v-ui:maxCardinality"][0] == 1,
			undef = $("li", control),
			langTag = $(".language-tag", control),
			language;

		if (isSingle && individual.hasValue(property_uri)) {
			language = individual[property_uri][0].language;
		}

		$("[bound]", control).data("language", language);
		langTag.text(language);

		language ? undef.removeClass("active") : undef.addClass("active");
		
		$(".language-list", control).append(
			Object.keys(veda.user.availableLanguages).map(function (language_name) {
				var li = undef.clone();
				$(".language", li).data("language", language_name).text(language_name).appendTo(li);
				language == language_name ? li.addClass("active") : li.removeClass("active");
				return li;
			})
		);

		$(".language", control).on("click", function ( e ) {
			e.preventDefault();
			var $this = $(this);
			$("[bound]", control)
				.data("language", $this.data("language") || null)
				.trigger("change");
		});

		function handler(doc_property_uri) {
			if (doc_property_uri === property_uri) {
				if ( isSingle && individual.hasValue(property_uri)) {
					language = individual[property_uri][0].language;
				}
				$(".language", control).map( function () {
					var $this = $(this);
					if ($this.data("language") == language) { 
						$this.parent().addClass("active");
						langTag.text(language || "");
					} else {
						$this.parent().removeClass("active");
					}
				});
			}
		}
		if (isSingle) {
			individual.on("individual:propertyModified", handler);
			this.one("remove", function () {
				individual.off("individual:propertyModified", handler);
			});
		}

		this.val = function (value) {
			if (!value) return $("[bound]", this).val();
			return $("[bound]", this).val(value);
		}
		return control;
	};
	veda_multilingual.defaults = {
		parser: function (input, el) {
			var value = new String(input);
			value.language = $(el).data("language") || undefined;
			return value != "" ? value : null;
		}
	};

	// Multilingual string control
	$.fn.veda_multilingualString = function (options) {
		var opts = $.extend( {}, $.fn.veda_multilingualString.defaults, options ),
			control = veda_multilingual.call(this, opts);
		this.append(control);
		return this;
	};
	$.fn.veda_multilingualString.defaults = {
		template: $("#multilingual-string-control-template").html(),
	};

	// Multilingual text control
	$.fn.veda_multilingualText = function (options) {
		var opts = $.extend( {}, $.fn.veda_multilingualText.defaults, options ),
			control = veda_multilingual.call(this, opts);
		this.append(control);
		return this;
	};
	$.fn.veda_multilingualText.defaults = {
		template: $("#multilingual-text-control-template").html(),
	};

	// BOOLEAN CHECKBOX CONTROL
	$.fn.veda_booleanCheckbox = function( options ) {
		var opts = $.extend( {}, $.fn.veda_booleanCheckbox.defaults, options ),
			control = $( opts.template ),
			input = $("[bound]", control),
			individual = opts.individual,
			property_uri = opts.property_uri,
			spec = opts.spec;

		function handler (doc_property_uri) {
			if (doc_property_uri === property_uri) {
				individual.hasValue(property_uri) && individual[property_uri][0] == true ? input.attr("checked", "checked") : input.removeAttr("checked");
			}
		}
		
		handler(property_uri);
		
		individual.on("individual:propertyModified", handler);
		this.one("remove", function () {
			individual.off("individual:propertyModified", handler);
		});
		
		input.on("change", function () {
			individual[property_uri] = [ new Boolean(input.is(":checked")) ];
		});
		
		this.on("view edit search", function (e) {
			e.stopPropagation();
			e.type === "view" ? input.attr("disabled", "disabled") : input.removeAttr("disabled");
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
			isSingle = spec && spec.hasValue("v-ui:maxCardinality") && spec["v-ui:maxCardinality"][0] == 1, 
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
			if (!value) return $("[bound]", this).val();
			return $("[bound]", this).val( value.toString() );
		}
		return control;
	};
	veda_selectLiteral.defaults = {
		template: $("#select-control-template").html(),
	}

	// Integer select control
	$.fn.veda_selectInteger = function (options) {
		var opts = $.extend( {}, $.fn.veda_selectInteger.defaults, options ),
			control = veda_selectLiteral.call(this, opts);
		this.append(control);
		return this;
	};
	$.fn.veda_selectInteger.defaults = {
		optionProperty: "v-ui:optionIntegerValue",
		parser: function (input) {
			var int = parseInt(input, 10);
			return !isNaN(int) ? new Number(int) : null;
		}
	};
	// Decimal select control
	$.fn.veda_selectDecimal = function (options) {
		var opts = $.extend( {}, $.fn.veda_selectDecimal.defaults, options ),
			control = veda_selectLiteral.call(this, opts);
		this.append(control);
		return this;
	};
	$.fn.veda_selectDecimal.defaults = {
		optionProperty: "v-ui:optionDecimalValue",
		parser: function (input) {
			var float = parseFloat(input.replace(",", "."));
			return !isNaN(float) ? new Number(float) : null;
		}
	};
	// String select control
	$.fn.veda_selectString = function (options) {
		var opts = $.extend( {}, $.fn.veda_selectString.defaults, options ),
			control = veda_selectLiteral.call(this, opts);
		this.append(control);
		return this;
	};
	$.fn.veda_selectString.defaults = {
		optionProperty: "v-ui:optionStringValue",
		parser: function (input, el) {
			var value = new String(input);
			return value != "" ? value : null;
		}
	};
	// Datetime select control
	$.fn.veda_selectDatetime = function (options) {
		var opts = $.extend( {}, $.fn.veda_selectDatetime.defaults, options ),
			control = veda_selectLiteral.call(this, opts);
		this.append(control);
		return this;
	};
	$.fn.veda_selectDatetime.defaults = {
		optionProperty: "v-ui:optionDatetimeValue",
		parser: function (input) {
			var timestamp = Date.parse(input);
			return !isNaN(timestamp) ? new Date(timestamp) : null;
		}
	};

	// CHECKBOX GROUP CONTROL
	
	// Checkbox group control
	$.fn.veda_checkboxGroup = function (options) {
		var opts = $.extend( {}, $.fn.veda_checkboxGroup.defaults, options ),
			control = veda_selectLiteral.call(this, opts);
		this.append(control);
		return this;
	};
	$.fn.veda_checkboxGroup.defaults = {
		parser: function (input) { 
			return input;
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
		if (property_uri === "v-s:script") opts.sourceMode = "javascript";
		if (property_uri === "v-ui:template") opts.sourceMode = "htmlmixed";
		var	editor = CodeMirror(editorEl, {
			value: opts.value,
			mode: opts.sourceMode,
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
		value: new String(""),
		template: $("#source-control-template").html(),
		mode: "javascript", 
		parser: function (input) {
			return new String(input);
		}
	};
	
	// OBJECT PROPERTY CONTROL
	$.fn.veda_link = function( options ) {
		var opts = $.extend( {}, $.fn.veda_link.defaults, options ),
			control = $(opts.template),
			queryPrefix = opts.queryPrefix,
			add = $(".add", control),
			all = $(".all", control);
		
		if (!opts.add) {
			add.remove();
		} else {
			add.click(opts.add);
		}

		if (!queryPrefix) {
			all.remove();
		} else {
			all.click(function () {
				typeAhead.data().ttTypeahead.input.trigger("queryChanged", "");
				typeAhead.focus();
			});
		}

		var typeAhead = $(".typeahead", control).typeahead (
			{
				minLength: 0,
				highlight: true
			},
			{
				name: "dataset",
				source: function (q, cb) {
					var limit = opts.limit || -1;
					var s = new veda.SearchModel(q, null, queryPrefix);
					var results = [];
					for (var uri in s.results) {
						if (limit-- === 0) break;
						results.push(s.results[uri]);
					}
					cb(results);
				},
				displayKey: function (individual) {
					var result;
					try { result = riot.render("{rdf:type.0.rdfs:label}: {rdfs:label}", individual); }
					catch (ex) { result = individual.id; }
					return result;
				}
			}
		);

		typeAhead.on("typeahead:selected", function (e, selected) {
			opts.select(selected);
		});

		// Search modal
		var tmpl = $("#search-modal-template").html();
		
		$(".search", control).on("click", function (e) {
			var $modal = $(tmpl);
			$("body").append($modal);
			$modal.modal();
			var search = new veda.SearchModel(undefined, $(".modal-body", $modal), queryPrefix);
			// Add found values
			$("button#ok", $modal).on("click", function (e) {
				$(this).off("click");
				var selected = [];
				for (var uri in search.selected) {
					selected.push( search.selected[uri] );
				}
				opts.select(selected);
			});
			$modal.on('hidden.bs.modal', function (e) {
				$modal.remove();
			});
		});
		
		this.append(control);
		return this;
	};
	$.fn.veda_link.defaults = {
		template: $("#link-control-template").html(),
		queryPrefix: ""
	};

})( jQuery );
