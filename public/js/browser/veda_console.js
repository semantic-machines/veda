;(function() { "use strict";
	if (window.performance.now) {
		var getTimestamp = function() { return window.performance.now(); };
	} else {
		if (window.performance.webkitNow) {
			var getTimestamp = function() { return window.performance.webkitNow(); };
		} else {
			var getTimestamp = function() { return new Date().getTime(); };
		}
	}
	$("#console #run").on("click", 
		function(event) {
			event.preventDefault();
			var t1 = getTimestamp();
			var 
				$runat = $("#console input[name='runat']:radio:checked"),
				$result = $("#console #result"), 
				$script = $("#console #script"),
				$output = $("#console #output");
			if ($runat.val() == "browser") {
				var res = eval($script.val());
				$result.val(res);
				$output.val("");
			} else {
				execute_script($script.val(), function(res) {
					$result.val(res[0]);
					$output.val(res[1]);
				});
			};
			var t2 = getTimestamp();
			$("#console #execution_time").html("(executed in " + (t2-t1).toFixed(2) + " msec)");
		});
})();
