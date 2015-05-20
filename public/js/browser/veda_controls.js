// Veda controls implemented as JQuery plugins
;(function( $ ) { "use strict";

	// Generic control behaviour
	$.fn.vedaControl = function( $el, options ) {
		var opts = $.extend( {}, $.fn.vedaControl.defaults, options ),
			control = $(opts.template),
			immutable = opts.immutable,
			view = $(".view", control),
			edit = $(".edit", control),
			add = $(".add", control),
			remove = $(".remove", control);
		
		if (!opts.add) add.remove();
		if (!opts.remove) remove.remove();
		
		$el
			.on("view edit search", function (e) { 
				e.stopPropagation();
				e.type === "view"   ? ( view.show(), edit.hide() ) : 
				e.type === "edit" && !immutable ? ( view.hide(), edit.show() ) : 
				e.type === "edit" && immutable ? ( view.show(), edit.hide() ) : 
				e.type === "search" ? ( view.hide(), edit.show() ) :
				true;
			})
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
	$.fn.vedaDatetime = function (options) {
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

	// Source code control
	$.fn.vedaSource = function (options) {
		var self = this,
			opts = $.extend( {}, $.fn.vedaSource.defaults, options ),
			immutable = opts.immutable,
			control = $(opts.template),
			fscreen = $("#full-screen", control),
			editorEl = control.get(0),
			editor = CodeMirror(editorEl, {
				value: opts.value.toString(),
				mode: opts.mode,
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
			e.type === "edit" && !immutable ? ( editor.setOption("readOnly", false) ) : 
			e.type === "edit" && immutable ? ( editor.setOption("readOnly", "nocursor") ) : 
			e.type === "search" ? ( editor.setOption("readOnly", false) ) :
			true;
		});
		editor.on("change", function () {
			var value = opts.inputParser( editor.doc.getValue() );
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
	$.fn.vedaSource.defaults = {
		value: new String(""),
		template: $("#source-control-template").html(),
		mode: "javascript", 
		inputParser: function (input) {
			var value = new String(input);
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
