// Console Presenter

function ConsolePresenter() { "use strict";
	// Get or create Model
	var cons = app.console || RegisterModule(Model(new ConsoleModel()), app, "console");

	// Get View
	var template = $("#console-template").html();
	var rendered = $.render(template, cons._properties);
	$("#main").html( rendered );
	
	// Bind UI changes to Model
	$("#console [bound]").on("change", function() {
		cons[this.id]($(this).val());
	});

	// Listen Model events
	cons.on("set", function(property, value) {
		var $el = $("#console #" + property);
		if ($el.is("input, textarea, select")) $el.val( value );
		else $el.html( value );
	});

	// Listen UI buttons clicks
	$("#console #run").on("click", function(event) {
		event.preventDefault();
		cons.run();
	});
	
	$("#console #reset").on("click", function(event) {
		event.preventDefault();
		cons.reset();
		$("#console #script").focus();
	});
};
