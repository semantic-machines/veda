;(function Console () { "use strict";
	$("#console button").on("click", 
		function(event) {
			event.preventDefault();
			execute_script($("#console #input").val(), function(result) {
				$("#console #output").val(result);
			});
		});
})();