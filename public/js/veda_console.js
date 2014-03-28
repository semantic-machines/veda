;(function() { "use strict";
	$("#console #run").on("click", 
		function(event) {
			event.preventDefault();
			execute_script($("#console #script").val(), function(result) {
				$("#console #result").val(result[0]);
				$("#console #output").val(result[1]);
			});
		});
})();
