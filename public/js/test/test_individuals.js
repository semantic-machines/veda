module('Individuals', {});

test(
		"Login",
		function() {
			ticket = authenticate("karpovr",
					"a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3").id;
			ok(ticket.length > 0);
		});

asyncTest(
		"Get individual 'owl:'",
		function() {
			ticket = authenticate("karpovr",
					"a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3").id;
			get_individual(ticket, "owl:", function(data) {
				ok(data["@"] == "owl:");
				start();
			});

		});

asyncTest(
		"Query '@' == 'owl:' ++ Get individual 'owl:'",
		function() {
			ticket = authenticate("karpovr",
					"a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3").id;
			query(ticket, "owl:", function(data) {
				ok(data.indexOf("owl:") >= 0);
				start();
			});

		});
