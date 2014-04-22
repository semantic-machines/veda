// Console Presenter

Veda(function ConsolePresenter(veda) { "use strict";
	
	veda.on("console:loaded", function (console) {
	
		// Render View
		var template = $("#console-template").html();
		var rendered = riot.render(template, console);
		$("#main").html( rendered );
		
		// Listen View changes & update Model
		$("#console [bound]").on("change", function() {
			console[this.id] = $(this).val();
		});
		$("#console #run").on("click", function(event) {
			event.preventDefault();
			console.run();
		});
		$("#console #reset").on("click", function(event) {
			event.preventDefault();
			console.reset();
			$("#console #script").focus();
		});
	
		// Listen Model changes & update View
		console.on("set", function(property, value) {
			var $el = $("#console #" + property);
			if ($el.is("input, textarea, select")) $el.val( value );
			else $el.html( value );
		});

	});

});
