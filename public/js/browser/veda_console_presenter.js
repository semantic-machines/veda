// veda_console Presenter
$(function() { "use strict";
	// get Model
	var template = $("#console-template").html();
	var cons = app.console ? app.console : Module(new ConsoleModel(), app, "console");
	cons.on("show", function() {
		$("#main").html(template);
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
});
