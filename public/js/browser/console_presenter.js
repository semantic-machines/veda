// Console Presenter

veda.Present(function Console(veda) { "use strict";

	//Get template
	var template = $("#console-template").html();
	var container = $("#main");
	
	veda.on("console:loaded", function (console) {
		
		container.hide().empty();
	
		// Render View
		var rendered = riot.render(template, console);
		container.html( rendered );
		//localize(container, veda.user.language);

		$("#console #runat option").each( function() {
			$(this).val() == console.runat ? $(this).attr("selected", "selected") : "";
		});
		
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
		console.on("property:changed", function(property, value) {
			var $el = $("#console #" + property + "[bound]");
			if ($el.is("input, textarea, select")) $el.val( value );
			else $el.html( value );
		});
		
		container.fadeIn(250);
		
	});

});
