;(function() { "use strict";
	$("#console #run").on("click", 
		function(event) {
			event.preventDefault();
			var 
				runat = $("#console input[name='runat']:radio:checked"),
				result = $("#console #result"), 
				script = $("#console #script"),
				output = $("#console #output");
			if (runat.val() == "browser") {
				var res = eval(script.val());
				result.val(res);
				output.val("");
			} else {
				execute_script(script.val(), function(res) {
					result.val(res[0]);
					output.val(res[1]);
				});
			};
		});
})();
