;(function() { "use strict";
	$("#console #run").on("click", 
		function(event) {
			event.preventDefault();
//			if ($("#console #clientside").attr("checked") == "checked") {
				var result = eval($("#console #script").val());
				$("#console #result").val(result);
//			} else {
//				execute_script($("#console #script").val(), function(result) {
//					$("#console #result").val(result[0]);
//					$("#console #output").val(result[1]);
//				});
//			};
		});
})();
