// veda_console Presenter
$(function() { "use strict";
	// initialize Model
	var cons = new veda_console();
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
		$("#console #result").html(cons.result);
		$("#console #output").val(cons.output); 
		$("#console #execution_time").html("(executed in " + (cons.stop - cons.start) + " msecs)");
	});
});