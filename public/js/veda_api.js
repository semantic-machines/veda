function Api () { "use strict";
	var self = $.observable(this);
	var ticket = {
		id: undefined,
		end_time: undefined,
		user_uri: undefined
	};
	var uri = uri || "owl:Ontology";
	var individual;
	authenticate("karpovr", "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3", function (ticket) {
		self.ticket = ticket;
		get_individual(ticket.id, uri, 0, function (individual) {
			self.individual = individual;
			$("#main-container").append(JSON.stringify(individual));
		});
	});
}