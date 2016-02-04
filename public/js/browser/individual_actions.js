/**
 * @class veda.IndividualActions
 * 
 * This class is used to bind additional business-logic events to individuals.
 */
veda.Module(function IndividualActions(veda) { "use strict";
	
	veda.on("individual:loaded", function (individual, container, template, mode) {
		function actionsHandler(template) {
			var $send = template.find("#send"); 
			var $sendButtons = template.find(".sendbutton"); 
			var $createReport = template.find("#createReport");
			var $createReportButtons = template.find(".create-report-button");
			var $showRights = template.find("#rightsOrigin");
			var $journal = template.find("#journal");
			
			function validHandler(e) { 
				$send.removeAttr("disabled");
				$sendButtons.removeAttr("disabled");
				$createReport.removeAttr("disabled");
				$createReportButtons.removeAttr("disabled");
				e.stopPropagation();
			}
			function inValidHandler(e) { 
				$send.attr("disabled", "disabled"); 
				$sendButtons.attr("disabled", "disabled"); 
				$createReport.attr("disabled", "disabled"); 
				$createReportButtons.attr("disabled", "disabled"); 
				e.stopPropagation();
			}
			template.on("valid", validHandler);
			template.on("invalid", inValidHandler);

			$send.on("click", function () { veda.Util.send(individual, template); });
			$createReport.on("click", function () {veda.Util.createReport(individual);});
			$showRights.on("click", function () {veda.Util.showRights(individual);});
			$journal.on("click", function() {
				var container = $('#main');
				container.empty();
				new veda.IndividualModel(individual.id+'j').present(container, undefined, 'view');
				changeHash(individual.id+'j');	
			});
			
			template.one("remove", function () {
				individual.off("individual:templateReady", actionsHandler);
			});
		}
		individual.on("individual:templateReady", actionsHandler);
	});
});
