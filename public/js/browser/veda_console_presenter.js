// Console Presenter

veda.ConsolePresenter = function() { "use strict";
	// Get or create Model
	var cons = veda.console || veda.RegisterModule(new veda.ConsoleModel(), veda, "console");

	// Get View
	var template = $("#console-template").html();
	var rendered = $.render(template, cons);
	$("#main").html( rendered );
	
	// Bind UI changes to Model
	$("#console [bound]").on("change", function() {
		cons[this.id] = $(this).val();
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
