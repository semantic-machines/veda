module('Individuals', {});

test(
		"Login",
		function() {
			ticket = authenticate("karpovr",
					"a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3").id;
			ok(ticket.length > 0);
		});

test(
		"Get individual 'owl:'",
		function() {
			ticket = authenticate("karpovr",
					"a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3").id;
			data = get_individual(ticket, "owl:");
			ok(data["@"] == "owl:");
		});

test(
		"Query '@' == 'owl:' ++ Get individual 'owl:'",
		function() {
			ticket = authenticate("karpovr",
					"a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3").id;
			data = query(ticket, "owl:");
			ok(data.indexOf("owl:") >= 0);
		});

test(
		"Individual store user1 and no read user2",
		function() {
			ticket_user1 = authenticate('bushenevv',
					'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
			ok(ticket_user1.id.length > 0);

			ticket_user2 = authenticate('BychinA',
					'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
			ok(ticket_user2.id.length > 0);

			new_test_doc1_uri = "test3:" + guid();
			new_test_doc1 = {
				'@' : new_test_doc1_uri,
				'rdf:type' : [ {
					data : 'veda-schema:document',
					type : 'Uri'
				} ],
				'veda-schema:author' : [ {
					data : 'mondi-data:ValeriyBushenev-Programmer1',
					type : 'Uri'
				} ],
				'veda-schema:test_field' : [ {
					data : 'test data',
					type : 'String'
				} ]
			};

			put_individual(ticket_user1.id, new_test_doc1);
			wait_pmodule(condition);
			wait_pmodule(acl_manager);

			read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			read_individual = get_individual(ticket_user2.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual) == false);
			
		});

test(
		"Individual store user1, add right and read user2",
		function() {
			ticket_user1 = authenticate('bushenevv',
					'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
			ok(ticket_user1.id.length > 0);

			ticket_user2 = authenticate('BychinA',
					'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
			ok(ticket_user2.id.length > 0);

			new_test_doc1_uri = guid();
			new_test_doc1 = {
				'@' : new_test_doc1_uri,
				'rdf:type' : [ {
					data : 'veda-schema:document',
					type : 'Uri'
				} ],
				'veda-schema:author' : [ {
					data : 'mondi-data:ValeriyBushenev-Programmer1',
					type : 'Uri'
				} ],
				'veda-schema:test_field' : [ {
					data : 'test data',
					type : 'String'
				} ]
			};

			put_individual(ticket_user1.id, new_test_doc1);
			wait_pmodule(condition);
			wait_pmodule(acl_manager);

			read_individual = get_individual(ticket_user2.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual) == false);

			read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			new_permission = {
				'@' : guid(),
				'rdf:type' : [ {
					data : 'veda-schema:PermissionStatement',
					type : 'Uri'
				} ],
				'veda-schema:canRead' : [ {
					data : 'true',
					type : 'String'
				} ],
				'veda-schema:permissionObject' : [ {
					data : new_test_doc1_uri,
					type : 'Uri'
				} ],
				'veda-schema:permissionSubject' : [ {
					data : ticket_user2.user_uri,
					type : 'Uri'
				} ]
			};
			put_individual(ticket_user1.id, new_permission); //
			wait_pmodule(2);

			read_individual = get_individual(ticket_user2.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));
			
		});

test(
		"Individual store user1 and read admin",
		function() {

			ticket_user1 = authenticate('bushenevv',
					'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
			ok(ticket_user1.id != "");

			admin_ticket = authenticate("karpovr",
					"a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3").id;

			new_test_doc1_uri = guid();
			new_test_doc1 = {
				'@' : new_test_doc1_uri,
				'rdf:type' : [ {
					data : 'veda-schema:document',
					type : 'Uri'
				} ],
				'veda-schema:author' : [ {
					data : 'mondi-data:ValeriyBushenev-Programmer1',
					type : 'Uri'
				} ],
				'veda-schema:test_field' : [ {
					data : 'test data',
					type : 'String'
				} ]
			};

			put_individual(ticket_user1.id, new_test_doc1);
			wait_pmodule(condition);
			wait_pmodule(acl_manager);

			read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);

			ok(compare(new_test_doc1, read_individual));

			read_individual = get_individual(admin_ticket, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));
			
		});

test("Individual store and read", function() {
	ticket = authenticate("karpovr",
					"a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3").id;
	new_test_doc1_uri = guid();
	new_test_doc1 = {
		'@' : new_test_doc1_uri,
		'rdf:type' : [ {
			data : 'veda-schema:PermissionStatement1',
			type : 'Uri'
		} ],
		'veda-schema:canDelete' : [ {
			data : 'true',
			type : 'String'
		} ],
		'veda-schema:canRead' : [ {
			data : 'true',
			type : 'String'
		} ],
		'veda-schema:canUpdate' : [ {
			data : 'true',
			type : 'String'
		} ],
		'veda-schema:permissionObject' : [ {
			data : 'individual_1',
			type : 'Uri'
		} ],
		'veda-schema:permissionSubject' : [ {
			data : 'individual_2',
			type : 'Uri'
		} ]
	};

	put_individual(ticket, new_test_doc1);
	wait_pmodule(subject_manager);

	read_individual = get_individual(ticket, new_test_doc1_uri);

	ok(compare(new_test_doc1, read_individual));
	
});

test("Individual of [veda-schema:PermissionStatement] store 3 and read 2",
		function() {
			ticket = authenticate("karpovr",
					"a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3").id;

			permissionSubject = guid();
			permissionObject = guid();

			new_test_doc1_uri = guid();
			new_test_doc1 = {
				'@' : new_test_doc1_uri,
				'rdf:type' : [ {
					data : 'veda-schema:PermissionStatement',
					type : 'Uri'
				} ],
				'veda-schema:canDelete' : [ {
					data : 'true',
					type : 'String'
				} ],
				'veda-schema:canRead' : [ {
					data : 'true',
					type : 'String'
				} ],
				'veda-schema:canUpdate' : [ {
					data : 'true',
					type : 'String'
				} ],
				'veda-schema:permissionObject' : [ {
					data : permissionObject,
					type : 'Uri'
				} ],
				'veda-schema:permissionSubject' : [ {
					data : permissionSubject,
					type : 'Uri'
				} ]
			};

			put_individual(ticket, new_test_doc1);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			new_test_doc2 = new_test_doc1;
			new_test_doc2_uri = guid();
			new_test_doc2['@'] = new_test_doc2_uri;
			new_test_doc2['veda-schema:canRead'] = [ {
				data : 'false',
				type : 'String'
			} ], put_individual(ticket, new_test_doc2);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket, new_test_doc2_uri);
			ok(compare(new_test_doc2, read_individual));

			new_test_doc3 = new_test_doc2;
			new_test_doc3_uri = guid();
			new_test_doc3['@'] = new_test_doc3_uri;
			new_test_doc3['veda-schema:canRead'] = [ {
				data : 'true',
				type : 'String'
			} ], put_individual(ticket, new_test_doc3);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket, new_test_doc3_uri);
			ok((read_individual['@'] == new_test_doc3_uri) == false);
			
		});

test(
		"Individual of [veda-schema:NoPermissionStatement] store 3 and read 3",
		function() {
			ticket = authenticate("karpovr",
					"a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3").id;

			permissionSubject = guid();
			permissionObject = guid();

			new_test_doc1_uri = guid();
			new_test_doc1 = {
				'@' : new_test_doc1_uri,
				'rdf:type' : [ {
					data : 'veda-schema:NoPermissionStatement',
					type : 'Uri'
				} ],
				'veda-schema:canDelete' : [ {
					data : 'true',
					type : 'String'
				} ],
				'veda-schema:canRead' : [ {
					data : 'true',
					type : 'String'
				} ],
				'veda-schema:canUpdate' : [ {
					data : 'true',
					type : 'String'
				} ],
				'veda-schema:permissionObject' : [ {
					data : permissionObject,
					type : 'Uri'
				} ],
				'veda-schema:permissionSubject' : [ {
					data : permissionSubject,
					type : 'Uri'
				} ]
			};

			put_individual(ticket, new_test_doc1);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			new_test_doc2 = new_test_doc1;
			new_test_doc2_uri = guid();
			new_test_doc2['@'] = new_test_doc2_uri;
			new_test_doc2['veda-schema:canRead'] = [ {
				data : 'false',
				type : 'String'
			} ], put_individual(ticket, new_test_doc2);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket, new_test_doc2_uri);
			ok(compare(new_test_doc2, read_individual));

			new_test_doc3 = new_test_doc2;
			new_test_doc3_uri = guid();
			new_test_doc3['@'] = new_test_doc3_uri;
			new_test_doc3['veda-schema:canRead'] = [ {
				data : 'true',
				type : 'String'
			} ], put_individual(ticket, new_test_doc3);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket, new_test_doc3_uri);
			ok((read_individual['@'] == new_test_doc3_uri) == true);
			
		});

test("Individual of [veda-schema:Membership] store 3 and read 2",
		function() {
			ticket = authenticate("karpovr",
					"a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3").id;

			memberOf = guid();
			resources = guid();

			new_test_doc1_uri = guid();
			new_test_doc1 = {
				'@' : new_test_doc1_uri,
				'rdf:type' : [ {
					data : 'veda-schema:Membership',
					type : 'Uri'
				} ],
				'veda-schema:memberOf' : [ {
					data : memberOf,
					type : 'Uri'
				} ],
				'veda-schema:resource' : [ {
					data : resources,
					type : 'Uri'
				} ]
			};

			put_individual(ticket, new_test_doc1);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			new_test_doc2 = new_test_doc1;
			new_test_doc2_uri = guid();
			new_test_doc2['@'] = new_test_doc2_uri;
			new_test_doc2['veda-schema:memberOf'] = [ {
				data : guid(),
				type : 'Uri'
			} ], put_individual(ticket, new_test_doc2);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket, new_test_doc2_uri);
			ok(compare(new_test_doc2, read_individual));

			new_test_doc3 = new_test_doc2;
			new_test_doc3_uri = guid();
			new_test_doc3['@'] = new_test_doc3_uri;
			new_test_doc3['veda-schema:memberOf'] = [ {
				data : memberOf,
				type : 'Uri'
			} ], put_individual(ticket, new_test_doc3);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket, new_test_doc3_uri);
			ok((read_individual['@'] == new_test_doc3_uri) == false);
			
		});

test("Individual of [veda-schema:NoMembership] store 3 and read 3",
		function() {
			ticket = authenticate("karpovr",
					"a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3").id;

			memberOf = guid();
			resources = guid();

			new_test_doc1_uri = guid();
			new_test_doc1 = {
				'@' : new_test_doc1_uri,
				'rdf:type' : [ {
					data : 'veda-schema:NoMembership',
					type : 'Uri'
				} ],
				'veda-schema:memberOf' : [ {
					data : memberOf,
					type : 'Uri'
				} ],
				'veda-schema:resource' : [ {
					data : resources,
					type : 'Uri'
				} ]
			};

			put_individual(ticket, new_test_doc1);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			new_test_doc2 = new_test_doc1;
			new_test_doc2_uri = guid();
			new_test_doc2['@'] = new_test_doc2_uri;
			new_test_doc2['veda-schema:memberOf'] = [ {
				data : guid(),
				type : 'Uri'
			} ], put_individual(ticket, new_test_doc2);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket, new_test_doc2_uri);
			ok(compare(new_test_doc2, read_individual));

			new_test_doc3 = new_test_doc2;
			new_test_doc3_uri = guid();
			new_test_doc3['@'] = new_test_doc3_uri;
			new_test_doc3['veda-schema:memberOf'] = [ {
				data : memberOf,
				type : 'Uri'
			} ], put_individual(ticket, new_test_doc3);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket, new_test_doc3_uri);
			ok((read_individual['@'] == new_test_doc3_uri) == true);
			
		});