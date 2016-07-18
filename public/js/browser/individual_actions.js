/**
 * @class veda.IndividualActions
 *
 * This class is used to bind additional business-logic events to individuals.
 */
veda.Module(function IndividualActions(veda) { "use strict";

	veda.on("individual:loaded", function (individual, container, templateOriginal, mode) {

		function actionsHandler(template) {

			template.one("remove", function () {
				individual.off("individual:templateReady", actionsHandler);
			});

			// Prevent excessive calls for individual that was displayed multiple times
			//if ( container !== "#main" && template !== templateOriginal) { return; }

			var $send = template.find("#send.action");
			var $sendButtons = template.find(".sendbutton");
			var $createReport = template.find("#createReport.action");
			var $createReportButtons = template.find(".create-report-button");
			var $showRights = template.find("#rightsOrigin.action");
			var $journal = template.find("#journal.action");

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
				var journal_uri = individual.id + "j",
						journal = new veda.IndividualModel(journal_uri);
				if (journal.hasValue("rdf:type") && journal["rdf:type"][0].id !== "rdfs:Resource") {
					riot.route("#/" + journal_uri);
				} else {
					alert("Журнал отсутсвует / Journal empty");
				}
			});
		}

		individual.on("individual:templateReady", actionsHandler);
	});

});
