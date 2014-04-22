// Console Presenter

veda(function ConsolePresenter(app) { "use strict";
	
	app.on("load:console", function () {
	
		// Get or create Model
		var cons = app.console || app.RegisterModule(new app.ConsoleModel(), app, "console");
	
		// Render View
		var template = $("#console-template").html();
		var rendered = riot.render(template, cons);
		$("#main").html( rendered );
		
		// Listen View changes & update Model
		$("#console [bound]").on("change", function() {
			cons[this.id] = $(this).val();
		});
		$("#console #run").on("click", function(event) {
			event.preventDefault();
			cons.run();
		});
		$("#console #reset").on("click", function(event) {
			event.preventDefault();
			cons.reset();
			$("#console #script").focus();
		});
	
		// Listen Model changes & update View
		cons.on("set", function(property, value) {
			var $el = $("#console #" + property);
			if ($el.is("input, textarea, select")) $el.val( value );
			else $el.html( value );
		});

	});

});
