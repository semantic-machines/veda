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
	// listen Model events
	console.on("run", function() {
		//$("form").children().addClass("disabled");
	});
	console.on("done", function() {
		//$("form").children().removeClass("disabled");
		$("#console #result").val(console.result); 
		$("#console #output").val(console.output); 
		$("#console #execution_time").html("(executed in " + (console.stop - console.start) + " msecs)");
	});
});