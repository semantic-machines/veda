// veda_console Presenter
$(function() { "use strict";
	// initialize Model
	console = new veda_console();
	// listen browser events
	$("#console #run").on("click", 
		function(event) {
			event.preventDefault();
			console.script = $("#console #script").val();
			console.runat = $("#console input[name='runat']:radio:checked").val();
			console.run();
		});
	$("#console #reset").on("click", 
		function(event) {
			$("#console #result").empty();
		});
	// listen Model events
	console.on("done", function() {
		$("#console #result").html(console.result);
		$("#console #output").val(console.output); 
		$("#console #execution_time").html("(executed in " + (console.stop - console.start) + " msecs)");
	});
});