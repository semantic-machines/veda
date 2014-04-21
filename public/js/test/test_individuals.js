'use strict';
module('Individuals', {});

var _admin_ticket = '';
var _user1_ticket = '';
var _user2_ticket = '';

function get_admin_ticket ()
{
    if (_admin_ticket == '')
    {
	return  _admin_ticket = authenticate("karpovr", "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3");	
    }
    return _admin_ticket;
}

function get_user1_ticket ()
{
    if (_user1_ticket == '')
    {
	_user1_ticket = authenticate("bushenevv", "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3");	
    }
    return _user1_ticket;
}


test(
		"#001 Login",
		function() {
			var ticket = get_admin_ticket ();
			ok(ticket.id.length > 0);
		});

test(
		"#002 Get individual 'owl:'",
		function() {
			var ticket = get_admin_ticket ();
			var data = get_individual(ticket.id, "owl:");
			ok(data["@"] == "owl:");
		});

test(
		"#003 Query '@' == 'owl:' ++ Get individual 'owl:'",
		function() {
			var ticket = get_admin_ticket ();
			var data = query(ticket.id, "owl:");
			ok(data.indexOf("owl:") >= 0);
		});

test(
		"#004 Individual store user1 and no read user2",
		function() {
			var ticket_user1 = get_user1_ticket ();
			ok(ticket_user1.id.length > 0);

			var ticket_user2 = authenticate('BychinA',
					'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
			ok(ticket_user2.id.length > 0);

			var new_test_doc1_uri = "test3:" + guid();
			var new_test_doc1 = {
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

			var read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			read_individual = get_individual(ticket_user2.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual) == false);
			
		});

test(
		"#005 Individual store user1, add right and read user2",
		function() {
			var ticket_user1 = get_user1_ticket ();
			ok(ticket_user1.id.length > 0);

			var ticket_user2 = authenticate('BychinA',
					'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
			ok(ticket_user2.id.length > 0);

			var new_test_doc1_uri = guid();
			var new_test_doc1 = {
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

			var read_individual = get_individual(ticket_user2.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual) == false);

			read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			var new_permission = {
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
		"#006 Individual store user1 and read admin",
		function() {

			var ticket_user1 = get_user1_ticket ();
			ok(ticket_user1.id != "");

			var a_ticket = get_admin_ticket ();

			var new_test_doc1_uri = guid();
			var new_test_doc1 = {
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

			var read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);

			ok(compare(new_test_doc1, read_individual));

			read_individual = get_individual(a_ticket.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));
			
		});

test("#007 Individual store and read", function() {
	var ticket = get_admin_ticket ();

	var new_test_doc1_uri = guid();
	var new_test_doc1 = {
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

	put_individual(ticket.id, new_test_doc1);
	wait_pmodule(subject_manager);

	var read_individual = get_individual(ticket.id, new_test_doc1_uri);

	ok(compare(new_test_doc1, read_individual));
	
});

test("#008 Individual of [veda-schema:PermissionStatement] store 3 and read 2",
		function() {
			var ticket = get_admin_ticket ();

			var permissionSubject = guid();
			var permissionObject = guid();

			var new_test_doc1_uri = guid();
			var new_test_doc1 = {
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

			put_individual(ticket.id, new_test_doc1);
			wait_pmodule(subject_manager);

			var read_individual = get_individual(ticket.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			var new_test_doc2 = new_test_doc1;
			var new_test_doc2_uri = guid();
			new_test_doc2['@'] = new_test_doc2_uri;
			new_test_doc2['veda-schema:canRead'] = [ {
				data : 'false',
				type : 'String'
			} ], put_individual(ticket.id, new_test_doc2);
			wait_pmodule(subject_manager);

			var read_individual = get_individual(ticket.id, new_test_doc2_uri);
			ok(compare(new_test_doc2, read_individual));

			var new_test_doc3 = new_test_doc2;
			var new_test_doc3_uri = guid();
			new_test_doc3['@'] = new_test_doc3_uri;
			new_test_doc3['veda-schema:canRead'] = [ {
				data : 'true',
				type : 'String'
			} ], put_individual(ticket.id, new_test_doc3);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket.id, new_test_doc3_uri);
			ok((read_individual['@'] == new_test_doc3_uri) == false);
			
		});

test(
		"#009 Individual of [veda-schema:NoPermissionStatement] store 3 and read 3",
		function() {
			var ticket = get_admin_ticket ();

			var permissionSubject = guid();
			var permissionObject = guid();

			var new_test_doc1_uri = guid();
			var new_test_doc1 = {
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

			put_individual(ticket.id, new_test_doc1);
			wait_pmodule(subject_manager);

			var read_individual = get_individual(ticket.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			var new_test_doc2 = new_test_doc1;
			var new_test_doc2_uri = guid();
			new_test_doc2['@'] = new_test_doc2_uri;
			new_test_doc2['veda-schema:canRead'] = [ {
				data : 'false',
				type : 'String'
			} ];
		        put_individual(ticket.id, new_test_doc2);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket.id, new_test_doc2_uri);
			ok(compare(new_test_doc2, read_individual));

			var new_test_doc3 = new_test_doc2;
			var new_test_doc3_uri = guid();
			new_test_doc3['@'] = new_test_doc3_uri;
			new_test_doc3['veda-schema:canRead'] = [ {
				data : 'true',
				type : 'String'
			} ];
			put_individual(ticket.id, new_test_doc3);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket.id, new_test_doc3_uri);
			ok((read_individual['@'] == new_test_doc3_uri) == true);
			
		});

test("#010 Individual of [veda-schema:Membership] store 3 and read 2",
		function() {
			var ticket = get_admin_ticket ();

			var memberOf = guid();
			var resources = guid();

			var new_test_doc1_uri = guid();
			var new_test_doc1 = {
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

			put_individual(ticket.id, new_test_doc1);
			wait_pmodule(subject_manager);

			var read_individual = get_individual(ticket.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			var new_test_doc2 = new_test_doc1;
			var new_test_doc2_uri = guid();
			new_test_doc2['@'] = new_test_doc2_uri;
			new_test_doc2['veda-schema:memberOf'] = [ {
				data : guid(),
				type : 'Uri'
			} ];
			put_individual(ticket.id, new_test_doc2);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket.id, new_test_doc2_uri);
			ok(compare(new_test_doc2, read_individual));

			var new_test_doc3 = new_test_doc2;
			var new_test_doc3_uri = guid();
			new_test_doc3['@'] = new_test_doc3_uri;
			new_test_doc3['veda-schema:memberOf'] = [ {
				data : memberOf,
				type : 'Uri'
			} ];
			put_individual(ticket.id, new_test_doc3);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket.id, new_test_doc3_uri);
			ok((read_individual['@'] == new_test_doc3_uri) == false);
			
		});

	test("#011 Individual of [veda-schema:NoMembership] store 3 and read 3",
		function() {
			var ticket = get_admin_ticket ();

			var memberOf = guid();
			var resources = guid();

			var new_test_doc1_uri = guid();
			var new_test_doc1 = {
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

			put_individual(ticket.id, new_test_doc1);
			wait_pmodule(subject_manager);

			var read_individual = get_individual(ticket.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			var new_test_doc2 = new_test_doc1;
			var new_test_doc2_uri = guid();
			new_test_doc2['@'] = new_test_doc2_uri;
			new_test_doc2['veda-schema:memberOf'] = [ {
				data : guid(),
				type : 'Uri'
			} ];
			put_individual(ticket.id, new_test_doc2);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket.id, new_test_doc2_uri);
			ok(compare(new_test_doc2, read_individual));

			var new_test_doc3 = new_test_doc2;
			var new_test_doc3_uri = guid();
			new_test_doc3['@'] = new_test_doc3_uri;
			new_test_doc3['veda-schema:memberOf'] = [ {
				data : memberOf,
				type : 'Uri'
			} ];
			put_individual(ticket.id, new_test_doc3);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket.id, new_test_doc3_uri);
			ok((read_individual['@'] == new_test_doc3_uri) == true);
			
		});

test(
		"#012 Individual store user1 and no search user2",
		function() {
			var ticket_user1 = get_user1_ticket ();
			ok(ticket_user1.id.length > 0);

			var ticket_user2 = authenticate('BychinA',
					'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
			ok(ticket_user2.id.length > 0);

			var new_test_doc1_uri = "test12:" + guid();
			var test_data = guid();
			var new_test_doc1 = {
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
					data : test_data,
					type : 'String'
				} ]
			};

			put_individual(ticket_user1.id, new_test_doc1);
//			wait_pmodule(subject_manager);
//			wait_pmodule(condition);
			wait_pmodule(fulltext_indexer);

			var data = query(ticket_user1.id, test_data);
			ok(compare(data.length, 1));

			var read_individual = get_individual(ticket_user2.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual) == false);
			
		});
