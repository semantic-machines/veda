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

function get_user2_ticket ()
{
    if (_user2_ticket == '')
    {
	_user2_ticket = authenticate("BychinA", "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3");	
    }
    return _user2_ticket;
}

var i = 0;

for (i = 0; i < 1; i++)
{

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
			var res = get_individual(ticket.id, "owl:");
			ok(res["@"] == "owl:");
			
			var labels = res["rdfs:label"];

			var ee;
			for (var key in labels)
			{    
			    if (labels[key].lang == "RU")
			    {
				ee = labels[key];
				break;
			    }
			}

			ok (ee != null);
			ok (ee.data == "Словарь OWL 2 Schema (OWL 2)");
		});

test(
		"#003 Query '@' == 'owl:' ++ Get individual 'owl:'",
		function() {
			var ticket = get_admin_ticket ();
			var data = query(ticket.id, "owl:");
			ok(data.indexOf("owl:") >= 0);
		});

test(
		"#004 Individual store user1 and no read user2, +lang",
		function() {
			var ticket_user1 = get_user1_ticket ();
			ok(ticket_user1.id.length > 0);

			var ticket_user2 = get_user2_ticket ();
			ok(ticket_user2.id.length > 0);

			var new_test_doc1_uri = "test3:" + guid();
			var new_test_doc1 = {
				'@' : new_test_doc1_uri,
				'rdf:type' : [ {
					data : 'v-s:Document',
					type : _Uri
				} ],
				'v-s:author' : [ {
					data : 'td:ValeriyBushenev-Programmer1',
					type : _Uri
				} ],
				'v-s:test_field' : [ {
					data : 'test data',
					lang : 'EN',
					type : _String
				} ]
			};

			put_individual(ticket_user1.id, new_test_doc1);
			wait_pmodule(condition);
			wait_pmodule(acl_manager);

			var read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			try { read_individual = get_individual(ticket_user2.id, new_test_doc1_uri); }
			catch (e) { read_individual = {}; }
			ok(compare(new_test_doc1, read_individual) == false);			
			
		});

test(
		"#005 Individual store user1, add right and read user2",
		function() {
			var ticket_user1 = get_user1_ticket ();
			ok(ticket_user1.id.length > 0);

			var ticket_user2 = get_user2_ticket ();
			ok(ticket_user2.id.length > 0);

			var new_test_doc1_uri = guid();
			var new_test_doc1 = {
				'@' : new_test_doc1_uri,
				'rdf:type' : [ {
					data : 'v-s:Document',
					type : _Uri
				} ],
				'v-s:author' : [ {
					data : 'td:ValeriyBushenev-Programmer1',
					type : _Uri
				} ],
				'v-s:test_field' : [ {
					data : 'test data',
					lang : 'NONE',
					type : _String
				} ]
			};

			put_individual(ticket_user1.id, new_test_doc1);
			wait_pmodule(condition);
			wait_pmodule(acl_manager);

			var read_individual;
			try { read_individual = get_individual(ticket_user2.id, new_test_doc1_uri); }
			catch (e) { read_individual = {}; }
			ok(compare(new_test_doc1, read_individual) == false);

			read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			var permission_uri = guid();

			var new_permission = {
				'@' : permission_uri,
				'rdf:type' : [ {
					data : 'v-s:PermissionStatement',
					type : _Uri
				} ],
				'v-s:canRead' : [ {
					data : true,
					type : _Bool
				} ],
				'v-s:permissionObject' : [ {
					data : new_test_doc1_uri,
					type : _Uri
				} ],
				'v-s:permissionSubject' : [ {
					data : ticket_user2.user_uri,
					type : _Uri
				} ]
			};
			put_individual(ticket_user1.id, new_permission); //
			wait_pmodule(2);

			read_individual = get_individual(ticket_user2.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			new_permission["@"] = "_";
			delete new_permission["v-s:permissionObject"];
			delete new_permission["v-s:permissionSubject"];

			var right1 = get_rights (ticket_user1.id, new_test_doc1_uri);
			var right2 = get_rights (ticket_user2.id, new_test_doc1_uri);

			ok(compare(new_permission, right2));

			new_permission['v-s:canUpdate'] = [{
					data : true,
					type : _Bool
				}];

			new_permission['v-s:canDelete'] = [{
					data : true,
					type : _Bool
				}];

			ok(compare(new_permission, right1));

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
					data : 'v-s:Document',
					type : _Uri
				} ],
				'v-s:author' : [ {
					data : 'td:ValeriyBushenev-Programmer1',
					type : _Uri
				} ],
				'v-s:test_field' : [ {
					data : 'test data',
					lang : 'NONE',
					type : _String
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

test("#007 Individual store and read, test datatype", function() {
	var ticket = get_admin_ticket ();

	var new_test_doc1_uri = guid();
	var new_test_doc1 = {
		'@' : new_test_doc1_uri,
		'rdf:type' : [ {
			data : 'v-s:test-data-types',
			type : _Uri
		} ],
		'v-s:test_integer' : [ {
			data : 9223372036854775295,
			type : _Integer
		} ],
		'v-s:test_negative_integer' : [ {
			data : -144365435,
			type : _Integer
		} ],
		'v-s:test_decimal' : [ {
			data : 12.12345678912345,
			type : _Decimal
		} ],
		'v-s:test_negative_decimal' : [ {
			data : -54.89764,
			type : _Decimal
		} ],
		'v-s:created' : [ {
			data : new Date (),
			type : _Datetime
		} ],
		'v-s:test_datetime0' : [ {
			data : new Date ("2014-01-02"),
			type : _Datetime
		} ],
		'v-s:test_datetime1' : [ {
			data : new Date ("2014-01-02T20:00"),
			type : _Datetime
		} ],
		'v-s:test_datetime2' : [ {
			data : new Date ("2014-01-02T20:10:24"),
			type : _Datetime
		} ],
		'v-s:test_datetime3' : [ {
			data : new Date ("2014-01-02T20:10:24.768"),
			type : _Datetime
		} ],
		'v-s:test_datetime4' : [ {
			data : new Date ("1960-01-02"),
			type : _Datetime
		} ],
		'v-s:canUpdate' : [ {
			data : true,
			type : _Bool
		} ],
		'v-s:permissionSubject' : [ {
			data : 'individual_2',
			type : _Uri
		} ]
	};

	put_individual(ticket.id, new_test_doc1);
	wait_pmodule(subject_manager);

	var read_individual = get_individual(ticket.id, new_test_doc1_uri);

	ok(compare(new_test_doc1, read_individual));
	
});

test("#008 Individual of [v-s:PermissionStatement] store 3 and read 2 (check on duplicate)",
		function() {
			var ticket = get_admin_ticket ();

			var permissionSubject = guid();
			var permissionObject = guid();

			var new_test_doc1_uri = guid();
			var new_test_doc1 = {
				'@' : new_test_doc1_uri,
				'rdf:type' : [ {
					data : 'v-s:PermissionStatement',
					type : _Uri
				} ],
				'v-s:canDelete' : [ {
					data : true,
					type : _Bool
				} ],
				'v-s:canRead' : [ {
					data : true,
					type : _Bool
				} ],
				'v-s:canUpdate' : [ {
					data : true,
					type : _Bool
				} ],
				'v-s:permissionObject' : [ {
					data : permissionObject,
					type : _Uri
				} ],
				'v-s:permissionSubject' : [ {
					data : permissionSubject,
					type : _Uri
				} ]
			};

			put_individual(ticket.id, new_test_doc1);
			wait_pmodule(subject_manager);

			var read_individual = get_individual(ticket.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			var new_test_doc2 = new_test_doc1;
			var new_test_doc2_uri = guid();
			new_test_doc2['@'] = new_test_doc2_uri;
			new_test_doc2['v-s:canRead'] = [ {
				data : false,
				type : _Bool
			} ];
			put_individual(ticket.id, new_test_doc2);
			wait_pmodule(subject_manager);

			var read_individual = get_individual(ticket.id, new_test_doc2_uri);
			ok(compare(new_test_doc2, read_individual));

			var new_test_doc3 = new_test_doc2;
			var new_test_doc3_uri = guid();
			new_test_doc3['@'] = new_test_doc3_uri;
			new_test_doc3['v-s:canRead'] = [ {
				data : true,
				type : _Bool
			} ];

			try { put_individual(ticket.id, new_test_doc3); } catch (err) {}

			wait_pmodule(subject_manager);

			try { read_individual = get_individual(ticket.id, new_test_doc3_uri); } catch (e) { read_individual = {}; }
			ok((read_individual['@'] == new_test_doc3_uri) == false);
			
		});

test(
		"#009 Individual of [v-s:NoPermissionStatement] store 3 and read 3",
		function() {
			var ticket = get_admin_ticket ();

			var permissionSubject = guid();
			var permissionObject = guid();

			var new_test_doc1_uri = guid();
			var new_test_doc1 = {
				'@' : new_test_doc1_uri,
				'rdf:type' : [ {
					data : 'v-s:NoPermissionStatement',
					type : _Uri
				} ],
				'v-s:canDelete' : [ {
					data : true,
					type : _Bool
				} ],
				'v-s:canRead' : [ {
					data : true,
					type : _Bool
				} ],
				'v-s:canUpdate' : [ {
					data : true,
					type : _Bool
				} ],
				'v-s:permissionObject' : [ {
					data : permissionObject,
					type : _Uri
				} ],
				'v-s:permissionSubject' : [ {
					data : permissionSubject,
					type : _Uri
				} ]
			};

			put_individual(ticket.id, new_test_doc1);
			wait_pmodule(subject_manager);

			var read_individual = get_individual(ticket.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			var new_test_doc2 = new_test_doc1;
			var new_test_doc2_uri = guid();
			new_test_doc2['@'] = new_test_doc2_uri;
			new_test_doc2['v-s:canRead'] = [ {
				data : false,
				type : _Bool
			} ];
		        put_individual(ticket.id, new_test_doc2);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket.id, new_test_doc2_uri);
			ok(compare(new_test_doc2, read_individual));

			var new_test_doc3 = new_test_doc2;
			var new_test_doc3_uri = guid();
			new_test_doc3['@'] = new_test_doc3_uri;
			new_test_doc3['v-s:canRead'] = [ {
				data : true,
				type : _Bool
			} ];
			put_individual(ticket.id, new_test_doc3);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket.id, new_test_doc3_uri);
			ok((read_individual['@'] == new_test_doc3_uri) == true);
			
		});

test("#010 Individual of [v-s:Membership] store 3 and read 2",
		function() {
			var ticket = get_admin_ticket ();

			var memberOf = guid();
			var resources = guid();

			var new_test_doc1_uri = guid();
			var new_test_doc1 = {
				'@' : new_test_doc1_uri,
				'rdf:type' : [ {
					data : 'v-s:Membership',
					type : _Uri
				} ],
				'v-s:memberOf' : [ {
					data : memberOf,
					type : _Uri
				} ],
				'v-s:resource' : [ {
					data : resources,
					type : _Uri
				} ]
			};

			put_individual(ticket.id, new_test_doc1);
			wait_pmodule(subject_manager);

			var read_individual = get_individual(ticket.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			var new_test_doc2 = new_test_doc1;
			var new_test_doc2_uri = guid();
			new_test_doc2['@'] = new_test_doc2_uri;
			new_test_doc2['v-s:memberOf'] = [ {
				data : guid(),
				type : _Uri
			} ];
			put_individual(ticket.id, new_test_doc2);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket.id, new_test_doc2_uri);
			ok(compare(new_test_doc2, read_individual));

			var new_test_doc3 = new_test_doc2;
			var new_test_doc3_uri = guid();
			new_test_doc3['@'] = new_test_doc3_uri;
			new_test_doc3['v-s:memberOf'] = [ {
				data : memberOf,
				type : _Uri
			} ];
			try { put_individual(ticket.id, new_test_doc3); } catch (err) {}
			wait_pmodule(subject_manager);

			try { read_individual = get_individual(ticket.id, new_test_doc3_uri); }
			catch (e) { read_individual = {}; }
			ok((read_individual['@'] == new_test_doc3_uri) == false);
			
		});

	test("#011 Individual of [v-s:NoMembership] store 3 and read 3",
		function() {
			var ticket = get_admin_ticket ();

			var memberOf = guid();
			var resources = guid();

			var new_test_doc1_uri = guid();
			var new_test_doc1 = {
				'@' : new_test_doc1_uri,
				'rdf:type' : [ {
					data : 'v-s:NoMembership',
					type : _Uri
				} ],
				'v-s:memberOf' : [ {
					data : memberOf,
					type : _Uri
				} ],
				'v-s:resource' : [ {
					data : resources,
					type : _Uri
				} ]
			};

			put_individual(ticket.id, new_test_doc1);
			wait_pmodule(subject_manager);

			var read_individual = get_individual(ticket.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			var new_test_doc2 = new_test_doc1;
			var new_test_doc2_uri = guid();
			new_test_doc2['@'] = new_test_doc2_uri;
			new_test_doc2['v-s:memberOf'] = [ {
				data : guid(),
				type : _Uri
			} ];
			put_individual(ticket.id, new_test_doc2);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket.id, new_test_doc2_uri);
			ok(compare(new_test_doc2, read_individual));

			var new_test_doc3 = new_test_doc2;
			var new_test_doc3_uri = guid();
			new_test_doc3['@'] = new_test_doc3_uri;
			new_test_doc3['v-s:memberOf'] = [ {
				data : memberOf,
				type : _Uri
			} ];
			put_individual(ticket.id, new_test_doc3);
			wait_pmodule(subject_manager);

			read_individual = get_individual(ticket.id, new_test_doc3_uri);
			ok((read_individual['@'] == new_test_doc3_uri) == true);
			
		});

test(
		"#012 user1 store 3 individuals (one of the individuals contains an invalid field [author]), the user1 finds 2 individuals, and the user2 does not find anything.",
		function() {
			var ticket_user1 = get_user1_ticket ();
			ok(ticket_user1.id.length > 0);

			var ticket_user2 = get_user2_ticket ();
			ok(ticket_user2.id.length > 0);

			var new_test_doc1_uri_1 = "test12:" + guid();

			var test_data_uid = guid();
			var test_data = 'testdata ' + test_data_uid;

			var new_test_doc1 = {
				'@' : new_test_doc1_uri_1,
				'rdf:type' : [ {
					data : 'v-s:Document',
					type : _Uri
				} ],
				'v-s:author' : [ {
					data : 'td:ValeriyBushenev-Programmer1',
					type : _Uri
				} ],
				'v-s:test_field' : [ {
					data : test_data,
					lang : 'NONE',
					type : _Uri
				} ],
				'v-s:test_fieldA' : [ {
					data : 'BBB' + test_data_uid,
					lang : 'NONE',
					type : _Uri
				} ],
				'v-s:test_fieldB' : [ {
					data : 'CCC' + test_data_uid,
					lang : 'NONE',
					type : _Uri
				} ]
			};

			// document content author != user1
			var new_test_doc1_uri_2 = "test12:" + guid();
			var new_test_doc2 = {
				'@' : new_test_doc1_uri_2,
				'rdf:type' : [ {
					data : 'v-s:Document',
					type : _Uri
				} ],
				'v-s:author' : [ {
					data : 'td:ValeriyBushenev-Programmer2',
					type : _Uri
				} ],
				'v-s:test_field' : [ {
					data : test_data,
					lang : 'NONE',
					type : _Uri
				} ]
			};

			var new_test_doc1_uri_3 = "test12:" + guid();
			var new_test_doc3 = {
				'@' : new_test_doc1_uri_3,
				'rdf:type' : [ {
					data : 'v-s:Document',
					type : _Uri
				} ],
				'v-s:author' : [ {
					data : 'td:ValeriyBushenev-Programmer1',
					type : _Uri
				} ],
				'v-s:test_field' : [ {
					data : test_data,
					lang : 'NONE',
					type : _Uri
				} ],
				'v-s:test_fieldA' : [ {
					data : 'BBB' + test_data_uid,
					lang : 'NONE',
					type : _Uri
				} ]
			};

			var new_test_doc1_uri_4 = "test12:" + guid();
			var new_test_doc4 = {
				'@' : new_test_doc1_uri_4,
				'rdf:type' : [ {
					data : 'v-s:Document',
					type : _Uri
				} ],
				'v-s:author' : [ {
					data : 'td:ValeriyBushenev-Programmer1',
					type : _Uri
				} ],
				'v-s:test_field' : [ {
					data : 'AAA' + test_data_uid,
					lang : 'NONE',
					type : _Uri
				} ],
				'v-s:test_fieldA' : [ {
					data : 'BBB' + test_data_uid,
					lang : 'NONE',
					type : _Uri
				} ],
				'v-s:test_fieldB' : [ {
					data : 'CCC' + test_data_uid,
					lang : 'NONE',
					type : _Uri
				} ]
			};

			put_individual(ticket_user1.id, new_test_doc1, false);
			put_individual(ticket_user1.id, new_test_doc2, false);
			put_individual(ticket_user1.id, new_test_doc3, false);
			put_individual(ticket_user1.id, new_test_doc4, false);

			wait_pmodule(fulltext_indexer);
			wait_pmodule(subject_manager);
			wait_pmodule(acl_manager);
			wait_pmodule(condition);			    

			var data = query(ticket_user1.id, test_data_uid, undefined, undefined, true);
			ok(compare(data.length, 2));

			data = query(ticket_user2.id,  test_data_uid, undefined, undefined, true);
			ok(compare(data.length, 0));

			data = query(ticket_user1.id,  "'v-s:test_field' == '" + test_data_uid + "'", undefined, undefined, true);
			ok(compare(data.length, 2));
			
			data = query(ticket_user1.id,  "'v-s:test_field1' == '" + test_data_uid + "'", undefined, undefined, true);
			ok(compare(data.length, 0));

			data = query(ticket_user1.id,  "'v-s:test_field1' == '" + test_data_uid + " t1'", undefined, undefined, true);
			ok(compare(data.length, 0));

			data = query(ticket_user1.id,  "'v-s:test_field' == '" + test_data_uid + "' || 'v-s:test_field' == 'AAA" + test_data_uid + "'", undefined, undefined, true);
			ok(compare(data.length, 3));

			data = query(ticket_user1.id,  "'v-s:test_fieldB' == 'CCC" + test_data_uid + "' && 'v-s:test_fieldA' == 'BBB" + test_data_uid + "'", undefined, undefined, true);
			ok(compare(data.length, 2));
		});

test(
		"#013 user1 store 5 individuals, ft search use range ",
		function() {
			var ticket_user1 = get_user1_ticket ();
			ok(ticket_user1.id.length > 0);

			var test_group_uid = guid();

			var new_test_doc1_uri = "test12:" + guid();
			var new_test_doc1 = {
				'@' : new_test_doc1_uri,
				'rdf:type' : [ {
					data : 'v-s:Document',
					type : _Uri
				} ],
				'v-s:author' : [ {
					data : 'td:ValeriyBushenev-Programmer1',
					type : _Uri
				} ],
				'v-s:created' : [ {
				data : new Date (),
				type : _Datetime
				} ],
				'v-s:test_group' : [ {
					data : test_group_uid,
					lang : 'NONE',
					type : _Uri
				} ],
				'v-s:test_datetime0' : [ {
				data : new Date ("2014-01-01"),
				type : _Datetime
				} ],
				'v-s:test_datetime1' : [ {
				data : new Date ("2014-05-01"),
				type : _Datetime
				} ]
			};

			var new_test_doc2_uri = "test12:" + guid();
			var new_test_doc2 = {
				'@' : new_test_doc2_uri,
				'rdf:type' : [ {
					data : 'v-s:Document',
					type : _Uri
				} ],
				'v-s:author' : [ {
					data : 'td:ValeriyBushenev-Programmer1',
					type : _Uri
				} ],
				'v-s:created' : [ {
				data : new Date (),
				type : _Datetime
				} ],
				'v-s:test_group' : [ {
					data : test_group_uid,
					lang : 'NONE',
					type : _Uri
				} ],
				'v-s:test_datetime0' : [ {
				data : new Date ("2014-01-02"),
				type : _Datetime
				} ],
				'v-s:test_datetime1' : [ {
				data : new Date ("2014-05-01"),
				type : _Datetime
				} ]
			};

			var new_test_doc3_uri = "test12:" + guid();
			var new_test_doc3 = {
				'@' : new_test_doc3_uri,
				'rdf:type' : [ {
					data : 'v-s:Document',
					type : _Uri
				} ],
				'v-s:author' : [ {
					data : 'td:ValeriyBushenev-Programmer1',
					type : _Uri
				} ],
				'v-s:created' : [ {
				data : new Date (),
				type : _Datetime
				} ],
				'v-s:test_group' : [ {
					data : test_group_uid,
					lang : 'NONE',
					type : _Uri
				} ],
				'v-s:test_datetime0' : [ {
				data : new Date ("2014-01-02"),
				type : _Datetime
				} ],
				'v-s:test_datetime1' : [ {
				data : new Date ("2014-06-11"),
				type : _Datetime
				} ]
			};

			var new_test_doc4_uri = "test12:" + guid();
			var new_test_doc4 = {
				'@' : new_test_doc4_uri,
				'rdf:type' : [ {
					data : 'v-s:Document',
					type : _Uri
				} ],
				'v-s:author' : [ {
					data : 'td:ValeriyBushenev-Programmer1',
					type : _Uri
				} ],
				'v-s:created' : [ {
				data : new Date (),
				type : _Datetime
				} ],
				'v-s:test_group' : [ {
					data : test_group_uid,
					lang : 'NONE',
					type : _Uri
				} ],
				'v-s:test_datetime0' : [ {
				data : new Date ("2014-01-04"),
				type : _Datetime
				} ],
				'v-s:test_datetime1' : [ {
				data : new Date ("2014-06-12"),
				type : _Datetime
				} ]
			};

			put_individual(ticket_user1.id, new_test_doc1, false);
			put_individual(ticket_user1.id, new_test_doc2, false);
			put_individual(ticket_user1.id, new_test_doc3, false);
			put_individual(ticket_user1.id, new_test_doc4, false);

			wait_pmodule(fulltext_indexer);
			wait_pmodule(subject_manager);
			wait_pmodule(acl_manager);
			wait_pmodule(condition);			    

			var data = query(ticket_user1.id, test_group_uid, undefined, undefined, true);
			ok(compare(data.length, 4));

			data = query(ticket_user1.id,  "'v-s:test_group' == '" + test_group_uid + "'", undefined, undefined,true);
			ok(compare(data.length, 4));
			
			data = query(ticket_user1.id,  
				     "'v-s:test_datetime0' == [2013-12-31T00:00:00, 2014-01-03T00:00:00] && 'v-s:test_group' == '" + test_group_uid + "'", undefined, undefined,true);
			ok(compare(data.length, 3));
			ok ((data[0] == new_test_doc1_uri || data[1] == new_test_doc1_uri || data[2] == new_test_doc1_uri) && 
			    (data[0] == new_test_doc2_uri || data[1] == new_test_doc2_uri || data[2] == new_test_doc2_uri) && 
			    (data[0] == new_test_doc3_uri || data[1] == new_test_doc3_uri || data[2] == new_test_doc3_uri));

			data = query(ticket_user1.id,  
				     "'v-s:test_datetime1' == [2014-04-01T00:00:00, 2014-06-03T00:00:00] && 'v-s:test_datetime0' == [2013-12-31T00:00:00, 2014-01-03T00:00:00] && 'v-s:test_group' == '" + test_group_uid + "'", undefined, undefined,true);
			ok(compare(data.length, 2));
			ok ((data[0] == new_test_doc1_uri || data[1] == new_test_doc1_uri) && (data[0] == new_test_doc2_uri || data[1] == new_test_doc2_uri));
		});


test(
		"#014 Individual store, add_to_individual, set_in_individual test",
		function() {
			var ticket_user1 = get_user1_ticket ();
			ok(ticket_user1.id.length > 0);

			var new_test_doc1_uri = "test14:" + guid();
			var new_test_doc1 = {
				'@' : new_test_doc1_uri,
				'rdf:type' : [ {
					data : 'v-s:Document',
					type : _Uri
				} ],
				'v-s:author' : [ {
					data : 'td:ValeriyBushenev-Programmer1',
					type : _Uri
				} ],
				'v-s:test_field' : [ {
					data : 'test data',
					lang : 'EN',
					type : _String
				} ]
			};

			put_individual(ticket_user1.id, new_test_doc1);
			wait_pmodule(condition);
			wait_pmodule(acl_manager);

			var read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);
			ok(compare(new_test_doc1, read_individual));

			var new_test_add1 = {
				'@' : new_test_doc1_uri,
				'v-s:author' : [ {
					data : 'td:ValeriyBushenev-Programmer2',
					type : _Uri
				}, 
				{
					data : 'td:test-q',
					type : _Uri
				} ]
			};

			add_to_individual(ticket_user1.id, new_test_add1);
			wait_pmodule(condition);
			wait_pmodule(acl_manager);

			var new_test_doc1_add1 = {
				'@' : new_test_doc1_uri,
				'rdf:type' : [ {
					data : 'v-s:Document',
					type : _Uri
				} ],
				'v-s:author' : [ {
					data : 'td:ValeriyBushenev-Programmer1',
					type : _Uri
				},
				{
					data : 'td:ValeriyBushenev-Programmer2',
					type : _Uri
				},
				{
					data : 'td:test-q',
					type : _Uri
				}
				 ],
				'v-s:test_field' : [ {
					data : 'test data',
					lang : 'EN',
					type : _String
				} ]
			};

			read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);
			ok(compare(new_test_doc1_add1, read_individual));

			var new_test_set1 = {
				'@' : new_test_doc1_uri,
				'v-s:author' : [ 
				{
					data : 'td:test-e',
					type : _Uri
				} ]
			};

			set_in_individual(ticket_user1.id, new_test_set1);
			wait_pmodule(condition);
			wait_pmodule(acl_manager);

			var new_test_doc1_set1 = {
				'@' : new_test_doc1_uri,
				'rdf:type' : [ {
					data : 'v-s:Document',
					type : _Uri
				} ],
				'v-s:author' : [ 
				{
					data : 'td:test-e',
					type : _Uri
				}
				 ],
				'v-s:test_field' : [ {
					data : 'test data',
					lang : 'EN',
					type : _String
				} ]
			};

			read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);
			ok(compare(new_test_doc1_set1, read_individual));
		});

}

