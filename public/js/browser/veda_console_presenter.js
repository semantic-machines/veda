// Console Presenter

function ConsolePresenter() { "use strict";
	// Get View
	var template = $("#console-template").html();

	// Get or create Model
	var cons = app.console || Module(new ConsoleModel(), app, "console");

	cons.on("show", function() {
		$("#main").html(template);
		$("#console #script").val(cons.script);
		$("#console #result").html(JSON.stringify(cons.result));
		$("#console #output").val(cons.output);
		$("#console #execution_time").html("(executed in " + (cons.stop - cons.start) + " msecs)");
		$("#console input[name='runat']").filter("[value='" + cons.runat + "']").attr('checked', true);
	});
	cons.trigger("show");

	// listen browser events
	$("#console #run").on("click", 
		function(event) {
			event.preventDefault();
			cons.script = $("#console #script").val();
			cons.runat = $("#console input[name='runat']:radio:checked").val();
			cons.run();
		});
	$("#console #reset").on("click", 
		function(event) {
			$("#console #result").empty();
		});

	// listen Model events
	cons.on("done", function() {
		$("#console #result").html(JSON.stringify(cons.result));
		$("#console #output").val(cons.output); 
		$("#console #execution_time").html("(executed in " + (cons.stop - cons.start) + " msecs)");
	});
}
