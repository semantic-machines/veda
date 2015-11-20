/**
 * @class veda.IndividualActions
 * 
 * This class is used to bind additional business-logic events to individuals.
 */
veda.Module(function IndividualActions(veda) { "use strict";
	
	veda.on("individual:loaded", function (individual, container, template, mode) {
		function actionsHandler(template) {
			var $send = template.find("#send"); 
			var $createReport = template.find("#createReport");
			var $showRights = template.find("#rightsOrigin");
			
			function validHandler(e) { 
				$send.removeAttr("disabled");
				$createReport.removeAttr("disabled");
				e.stopPropagation();
			}
			function inValidHandler(e) { 
				$send.attr("disabled", "disabled"); 
				$createReport.attr("disabled", "disabled"); 
				e.stopPropagation();
			}
			template.on("valid", validHandler);
			template.on("invalid", inValidHandler);

			$send.on("click", function () { veda.Util.send(individual, template); });
			$createReport.on("click", function () {veda.Util.createReport(individual);});
			$showRights.on("click", function () {veda.Util.showRights(individual);});
			
			template.one("remove", function () {
				individual.off("individual:templateReady", actionsHandler);
			});
		}
		individual.on("individual:templateReady", actionsHandler);
	});
});
