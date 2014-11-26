// Veda controls implemented as JQuery plugins
(function( $ ) {

	// Generic control behaviour
	$.fn.vedaControl = function( jq, options ) {

		var opts = $.extend( {}, $.fn.vedaControl.defaults, options );
		
		var control = $(opts.template);
		
		view = $(".view", control),
		edit = $(".edit", control);

		jq.on("view", function () {
			view.show();
			edit.hide();
		});
		jq.on("edit", function () {
			view.hide();
			edit.show();
		});

		$(".remove", control).on("click", function () {
			jq.empty().remove();
			opts.remove();
		});

		$(".add", control).on("click", function () {
			opts.add();
		});

		return control;
		
	}
	
	$.fn.vedaControl.defaults = {
		add: function () { alert("add") },
		remove: function () { alert("remove") },
		change: function (value) { alert(value) },
	}

	// String control
	$.fn.vedaString = function( options ) {
		
		var opts = $.extend( {}, $.fn.vedaString.defaults, options );

		var control = $.fn.vedaControl(this, opts);
		
		$("[bound]", control)
			.html(opts.value)
			.val(opts.value)
			.data("language", opts.value.language)
			.on("change", function ( e ) {
				var value = new String(this.value);
				value.language = $(this).data("language");
				opts.change(value);
			});							
		
		$(".language-selector", control).prepend(opts.value.language);
		
		var first = $("<li>").append( $("<a>", {href: "#", "data-language": "", text: "-"}).addClass("language") );
		if (!opts.value.language) first.addClass("active");
		$(".language-list", control).append(
			first,
			Object.keys(Veda().availableLanguages).map(function (language_name) {
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
			$("textarea", control)
				.data("language", $(this).data("language") )
				.trigger("change");
		});
		
		$("textarea", control)
			.autosize()
			.on("focus", function (event) {
				$(this).trigger("autosize.resize");
			});

		this.append(control);
		
		return this;
	};

	$.fn.vedaString.defaults = {
		value: new String(""),
		template: $("#string-control-template").html(),
		change: function (value) { alert(value) }
	};
	
	// Boolean control	
	$.fn.vedaBoolean = function( options ) {
		
		var opts = $.extend( {}, $.fn.vedaBoolean.defaults, options );

		var control = $.fn.vedaControl(this, opts);
		
		$("[bound]", control)
			.html(opts.value)
			.val(opts.value)
			.on("change", function ( e ) {
				var value = new Boolean(this.value == "true" ? true : false);
				opts.change(value);
			});						
		
		this.append(control);
		
		return this;
	};

	$.fn.vedaBoolean.defaults = {
		value: new Boolean(false),
		template: $("#boolean-control-template").html(),
		change: function (value) { alert(value) }
	};	
	
	// Integer control
	$.fn.vedaInteger = function( options ) {
		
		var opts = $.extend( {}, $.fn.vedaInteger.defaults, options );

		var control = $.fn.vedaControl(this, opts);
		
		$("[bound]", control)
			.html(opts.value)
			.val(opts.value)
			.on("change", function ( e ) {
				var value = "", 
					int = parseInt(this.value, 10);
				if ( isNaN(int) == false ) value = new Number(int);
				opts.change(value);
			});						
		
		this.append(control);
		
		return this;
	};

	$.fn.vedaInteger.defaults = {
		value: undefined,
		template: $("#integer-control-template").html(),
		change: function (value) { alert(value) }
	};

	// Decimal control
	$.fn.vedaDecimal = function( options ) {
		
		var opts = $.extend( {}, $.fn.vedaDecimal.defaults, options );

		var control = $.fn.vedaControl(this, opts);
		
		$("[bound]", control)
			.html(opts.value)
			.val(opts.value)
			.on("change", function ( e ) {
				var value = "", 
					float = parseFloat(this.value);
				if ( isNaN(float) == false ) value = new Number(float);
				opts.change(value);
			});						
		
		this.append(control);
		
		return this;
	};

	$.fn.vedaDecimal.defaults = {
		value: undefined,
		template: $("#decimal-control-template").html(),
		change: function (value) { alert(value) }
	};

	// Datetime control
	$.fn.vedaDatetime = function( options ) {
		
		var opts = $.extend( {}, $.fn.vedaDatetime.defaults, options );

		var control = $.fn.vedaControl(this, opts);
		
		$("[bound]", control)
			.html(opts.value)
			.val(opts.value)
			.on("change", function ( e ) {
				var value = "", 
					timestamp = Date.parse(this.value);
				if ( isNaN(timestamp) == false ) value = new Date(timestamp);
				opts.change(value);
			});						
		
		this.append(control);
		
		return this;
	};

	$.fn.vedaDatetime.defaults = {
		value: undefined,
		template: $("#datetime-control-template").html(),
		change: function (value) { alert(value) }
	};

})( jQuery );

