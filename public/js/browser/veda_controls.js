// Veda controls implemented as JQuery plugins
;(function( $ ) { "use strict";

	// Generic control behaviour
	$.fn.vedaControl = function( $el, options ) {
		var opts = $.extend( {}, $.fn.vedaControl.defaults, options ),
			control = $(opts.template),
			view = $(".view", control),
			edit = $(".edit:not(.search)", control),
			search = $(".search:not(.edit)", control),
			edit_search = $(".edit.search", control),
			add = $(".add", control),
			remove = $(".remove", control),
			mode = "";
		
		if (!opts.add) add.remove();
		if (!opts.remove) remove.remove();
		
		$el
			.on("view", function () { mode="view"; view.show(); edit.hide(); search.hide(); edit_search.hide(); })
			.on("edit", function () { mode="edit"; view.hide(); edit.show(); search.hide(); edit_search.show(); })
			.on("search", function () { mode="search"; view.hide(); edit.hide(); search.show(); edit_search.show(); })
			.mouseenter( function () { add.show(); remove.show(); })
			.mouseleave( function () { add.hide(); remove.hide(); })
			.on("focusin", function () {
				$el.off("mouseenter mouseleave");
				add.show();
				remove.show();
			})
			.on("focusout", function () {
				$el
					.mouseenter(function () {
						add.show();
						remove.show();
					})
					.mouseleave(function () {
						add.hide();
						remove.hide();
					});
			});

		remove
			.hide()
			.on("click", function () {
				$el.empty().remove();
				opts.remove();
			});
		add
			.hide()
			.on("click", function () {
				opts.add();
			});
		$("[bound]", control)
			.text(opts.value)
			.val(opts.value)
			.on("change", function ( e ) {
				var value = opts.inputParser( this.value, this );
				view
					.val( value )
					.text( value );
				opts.change(value);
			});	
		
		return control;
	};
	$.fn.vedaControl.defaults = {
		/*add: function () { alert("add") },
		remove: function () { alert("remove") },
		change: function (value) { alert(value) },*/
		inputParser: function (input) { return input; },
		value: undefined
	};

	// String control
	$.fn.vedaString = function( options ) {
		var opts = $.extend( {}, $.fn.vedaString.defaults, options ),
			control = $.fn.vedaControl(this, opts);

		$("[bound]", control).data("language", opts.value.language);

		$(".language-selector", control).prepend(opts.value.language);

		var first = $("<li>").append( $("<a>", {href: "#", "data-language": "", text: "-"}).addClass("language") );
		if (!opts.value.language) first.addClass("active");
		$(".language-list", control).append(
			first,
			Object.keys(veda.user.availableLanguages).map(function (language_name) {
				var li = $("<li>"), 
					a = $("<a>", {href: "#", "data-language": language_name, text: language_name}).addClass("language");
				li.append(a);
				if (opts.value.language == language_name) li.addClass("active");
				return li;
			})
		);
		
		$(".language", control).on("click", function ( e ) {
			e.preventDefault();
			$(".language-selector", control)
				.empty()
				.append($(this).data("language"), " <span class='caret'></span>");
			$(".language-list li", control).removeClass("active");
			$(this).parent().addClass("active");
			$("textarea", control)
				.data("language", $(this).data("language") )
				.trigger("change");
		});
		
		/*$("textarea", control)
			.autosize()
			.on("focus", function (event) {
				$(this).trigger("autosize.resize");
			});*/

		this.append(control);
		return this;
	};
	$.fn.vedaString.defaults = {
		value: new String(""),
		template: $("#string-control-template").html(),
		//change: function (value) { alert(value + " : " + value.language) },
		inputParser: function (input, el) {
			var value = new String(input);
			value.language = $(el).data("language");
			return value;
		}
	};
	
	// Boolean control	
	$.fn.vedaBoolean = function( options ) {
		var opts = $.extend( {}, $.fn.vedaBoolean.defaults, options ),
			control = $.fn.vedaControl(this, opts);
		this.append(control);
		return this;
	};
	$.fn.vedaBoolean.defaults = {
		value: new Boolean(false),
		template: $("#boolean-control-template").html(),
		inputParser: function (input) {
			var value = new Boolean(input == "true" ? true : false);
			return value;
		}
	};	

	// Integer control
	$.fn.vedaInteger = function( options ) {
		var opts = $.extend( {}, $.fn.vedaInteger.defaults, options ),
			control = $.fn.vedaControl(this, opts);
		this.append(control);
		return this;
	};
	$.fn.vedaInteger.defaults = {
		value: undefined,
		template: $("#integer-control-template").html(),
		inputParser: function (input) {
			var value = "", 
				int = parseInt(input, 10);
			if ( isNaN(int) === false ) value = new Number(int);
			return value;
		}
	};

	// Decimal control
	$.fn.vedaDecimal = function( options ) {
		var opts = $.extend( {}, $.fn.vedaDecimal.defaults, options ),
			control = $.fn.vedaControl(this, opts);
		this.append(control);
		return this;
	};
	$.fn.vedaDecimal.defaults = {
		value: undefined,
		template: $("#decimal-control-template").html(),
		inputParser: function (input) {
			var value = "", 
				float = parseFloat(input);
			if ( isNaN(float) === false ) value = new Number(float);
			return value;
		}
	};

	// Datetime control
	$.fn.vedaDatetime = function( options ) {
		var opts = $.extend( {}, $.fn.vedaDatetime.defaults, options ),
			control = $.fn.vedaControl(this, opts);
		this.append(control);
		return this;
	};
	$.fn.vedaDatetime.defaults = {
		value: undefined,
		template: $("#datetime-control-template").html(),
		inputParser: function (input) {
			var value = "", 
				timestamp = Date.parse(input);
			if ( isNaN(timestamp) === false ) value = new Date(timestamp);
			return value;
		}
	};
	
	// Object property control
	$.fn.vedaLink = function( options ) {
		var opts = $.extend( {}, $.fn.vedaLink.defaults, options ),
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
	$.fn.vedaLink.defaults = {
		template: $("#link-control-template").html(),
		queryPrefix: ""
	};

})( jQuery );
