'use strict';
module('Individuals',
{});

var _admin_ticket = '';
var _user1_ticket = '';
var _user2_ticket = '';
var _event_id;

function get_admin_ticket()
{
    if (_admin_ticket == '')
    {
        return _admin_ticket = authenticate("karpovrt", "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3");
    }
    return _admin_ticket;
}

function get_user1_ticket()
{
    if (_user1_ticket == '')
    {
        _user1_ticket = authenticate("bushenevvt", "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3");
    }
    return _user1_ticket;
}

function get_user2_ticket()
{
    if (_user2_ticket == '')
    {
        _user2_ticket = authenticate("BychinAt", "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3");
    }
    return _user2_ticket;
}

/*
function wait_module(module_id, op_id)
{
	if (!op_id)
		return;
		
	for (var i = 0; i < 100; i++)
	{
		var in_module_op_id = get_operation_state(module_id);
	
		if (in_module_op_id >= op_id || in_module_op_id == -1)
			return;
	}		
}
*/
var i = 0;

for (i = 0; i < 1; i++)
{

    test(
        "#001 Login",
        function()
        {
            var ticket = get_admin_ticket();
            ok(ticket.id.length > 0);
        });

    test(
        "#002 Get individual 'owl:'",
        function()
        {
            var ticket = get_admin_ticket();
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

            ok(ee != null);
            ok(ee.data == "Словарь OWL 2 Schema (OWL 2)");
        });

    test(
        "#003 Query '@' == 'owl:' ++ Get individual 'owl:'",
        function()
        {
            var ticket = get_admin_ticket();
            var data = query(ticket.id, "owl:");
            ok(data.indexOf("owl:") >= 0);
        });

    test(
        "#004 Individual store user1 and no read user2, +lang",
        function()
        {
            var ticket_user1 = get_user1_ticket();
            ok(ticket_user1.id.length > 0);

            var ticket_user2 = get_user2_ticket();
            ok(ticket_user2.id.length > 0);

            var new_test_doc1_uri = "test3:" + guid();
            var new_test_doc1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri ('v-s:Document1'),
                'v-s:author': newUri ('td:ValeriyBushenev-Programmer1'),
                'v-s:test_field': newStr ('test data', 'EN')
            };

            var res = put_individual(ticket_user1.id, new_test_doc1);
            wait_module(condition, res.op_id);
            wait_module(acl_manager, res.op_id);

            var read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);
            ok(compare(new_test_doc1, read_individual));

            try
            {
                read_individual = get_individual(ticket_user2.id, new_test_doc1_uri);
            }
            catch (e)
            {
                read_individual = {};
            }
            ok(compare(new_test_doc1, read_individual) == false);

        });

    test(
        "#005 Individual store user1 and add right, user2 successfully read it, next user1 add denied right and no user2 fail read it",
        function()
        {
            var ticket_user1 = get_user1_ticket();
            ok(ticket_user1.id.length > 0);

            var ticket_user2 = get_user2_ticket();
            ok(ticket_user2.id.length > 0);

            var new_test_doc1_uri = guid();
            var new_test_doc1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri ('v-s:Document1'),
                'v-s:author': newUri ('td:ValeriyBushenev-Programmer1'),
                'v-s:test_field': newStr ('test data', 'NONE')
            };

            var res = put_individual(ticket_user1.id, new_test_doc1);
            wait_module(condition, res.op_id);
            wait_module(acl_manager, res.op_id);

            var read_individual;
            try
            {
                read_individual = get_individual(ticket_user2.id, new_test_doc1_uri);
            }
            catch (e)
            {
                read_individual = {};
            }
            ok(compare(new_test_doc1, read_individual) == false);

            read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);
            ok(compare(new_test_doc1, read_individual));

            var res = addRight(ticket_user1.id, [can_read], ticket_user2.user_uri, new_test_doc1_uri);
            var new_permission = res[0];
            wait_module(acl_manager, res[1].op_id);

            read_individual = get_individual(ticket_user2.id, new_test_doc1_uri);
            ok(compare(new_test_doc1, read_individual));

            new_permission["@"] = "_";
            delete new_permission["v-s:permissionObject"];
            delete new_permission["v-s:permissionSubject"];

            var right1 = get_rights(ticket_user1.id, new_test_doc1_uri);
            var right2 = get_rights(ticket_user2.id, new_test_doc1_uri);

            ok(compare(new_permission, right2));

            new_permission['v-s:canUpdate'] = newBool(true);
            new_permission['v-s:canDelete'] = newBool(true);

            ok(compare(new_permission, right1));

            res = addRight(ticket_user1.id, [cant_read], ticket_user2.user_uri, new_test_doc1_uri);
            wait_module(acl_manager, res[1].op_id);

            try
            {
                read_individual = get_individual(ticket_user2.id, new_test_doc1_uri);
            }
            catch (e)
            {
                read_individual = {};
            }
            ok(compare(new_test_doc1, read_individual) == false);
        });

    test(
        "#006 Individual store user1 and read admin",
        function()
        {

            var ticket_user1 = get_user1_ticket();
            ok(ticket_user1.id != "");

            var a_ticket = get_admin_ticket();

            var new_test_doc1_uri = guid();
            var new_test_doc1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri ('v-s:Document1'),
                'v-s:author': newUri ('td:ValeriyBushenev-Programmer1'),
                'v-s:test_field': newStr ('test data', 'NONE')
            };

            var res = put_individual(ticket_user1.id, new_test_doc1);
            wait_module(condition, res.op_id);
            wait_module(acl_manager, res.op_id);

            var read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);

            ok(compare(new_test_doc1, read_individual));

            read_individual = get_individual(a_ticket.id, new_test_doc1_uri);
            ok(compare(new_test_doc1, read_individual));

        });

    test("#007 Individual store and read, test datatype", function()
    {
        var ticket = get_admin_ticket();

        var new_test_doc1_uri = guid();
        var new_test_doc1 = {
            '@': new_test_doc1_uri,
            'rdf:type': newUri ('v-s:test-data-types'),
            'v-s:test_integer': newInt (9223372036854775295),
            'v-s:test_negative_integer': newInt (-144365435),
            'v-s:test_decimal': newDecimal (12.12345678912345),
            'v-s:test_negative_decimal': newDecimal (-54.89764),
            'v-s:test_decimal2': newDecimal (0.7),
            'v-s:test_decimal3': newDecimal (764.3),
            'v-s:test_decimal4': newDecimal (90.8),
            'v-s:test_decimal5': newDecimal (7.6),
            'v-s:test_decimal6': newDecimal (0.07),
            'v-s:test_decimal7': newDecimal (0.007),
            'v-s:created': newDate (new Date()),
            'v-s:test_datetime0': newDate (new Date("2014-01-02")),
            'v-s:test_datetime1': newDate (new Date("2014-01-02T20:00")),
            'v-s:test_datetime2': newDate (new Date("2014-01-02T20:10:24")),
            'v-s:test_datetime3': newDate (new Date("2014-01-02T20:10:24.768")),
            'v-s:test_datetime4': newDate (new Date("1960-01-02")),
            'v-s:canUpdate': newBool (true),
            'v-s:permissionSubject': newUri ('individual_2')
        };

        var res = put_individual(ticket.id, new_test_doc1);
        wait_module(subject_manager, res.op_id);

        var read_individual = get_individual(ticket.id, new_test_doc1_uri);
        ok(compare(new_test_doc1, read_individual));
    });

    test("#008 Individual of [v-s:PermissionStatement] store 3 and read 2 (check on duplicate)",
        function()
        {
            var ticket = get_admin_ticket();

            var permissionSubject = guid();
            var permissionObject = guid();

            var new_test_doc1_uri = guid();
            var new_test_doc1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri ('v-s:PermissionStatement'),
                'v-s:canDelete': newBool (true),
                'v-s:canRead': newBool (true),
                'v-s:canUpdate': newBool (true),
                'v-s:permissionObject': newUri (permissionObject),
                'v-s:permissionSubject': newUri (permissionSubject)
            };

            var res = put_individual(ticket.id, new_test_doc1);
            wait_module(subject_manager, res.op_id);

            var read_individual = get_individual(ticket.id, new_test_doc1_uri);
            ok(compare(new_test_doc1, read_individual));

            var new_test_doc2 = new_test_doc1;
            var new_test_doc2_uri = guid();
            new_test_doc2['@'] = new_test_doc2_uri;
            new_test_doc2['v-s:canRead'] = newBool (false);
            var res = put_individual(ticket.id, new_test_doc2);
            wait_module(subject_manager, res.op_id);

            var read_individual = get_individual(ticket.id, new_test_doc2_uri);
            ok(compare(new_test_doc2, read_individual));

            var new_test_doc3 = new_test_doc2;
            var new_test_doc3_uri = guid();
            new_test_doc3['@'] = new_test_doc3_uri;
            new_test_doc3['v-s:canRead'] = newBool (true);

            try
            {
                res = put_individual(ticket.id, new_test_doc3);
            }
            catch (err)
            {}

            wait_module(subject_manager, res.op_id);

            try
            {
                read_individual = get_individual(ticket.id, new_test_doc3_uri);
            }
            catch (e)
            {
                read_individual = {};
            }
            ok((read_individual['@'] == new_test_doc3_uri) == false);

        });

    test(
        "#009 Individual of [v-s:NoPermissionStatement] store 3 and read 3",
        function()
        {
            var ticket = get_admin_ticket();

            var permissionSubject = guid();
            var permissionObject = guid();

            var new_test_doc1_uri = guid();
            var new_test_doc1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri ('v-s:NoPermissionStatement'),
                'v-s:canDelete': newBool (true),
                'v-s:canRead': newBool (true),
                'v-s:canUpdate': newBool (true),
                'v-s:permissionObject': newUri (permissionObject),
                'v-s:permissionSubject': newUri (permissionSubject)
            };

            var res = put_individual(ticket.id, new_test_doc1);
            wait_module(subject_manager, res.op_id);

            var read_individual = get_individual(ticket.id, new_test_doc1_uri);
            ok(compare(new_test_doc1, read_individual));

            var new_test_doc2 = new_test_doc1;
            var new_test_doc2_uri = guid();
            new_test_doc2['@'] = new_test_doc2_uri;
            new_test_doc2['v-s:canRead'] = newBool (false);
            var res = put_individual(ticket.id, new_test_doc2);
            wait_module(subject_manager, res.op_id);

            read_individual = get_individual(ticket.id, new_test_doc2_uri);
            ok(compare(new_test_doc2, read_individual));

            var new_test_doc3 = new_test_doc2;
            var new_test_doc3_uri = guid();
            new_test_doc3['@'] = new_test_doc3_uri;
            new_test_doc3['v-s:canRead'] = newBool (true);
            var res = put_individual(ticket.id, new_test_doc3);
            wait_module(subject_manager, res.op_id);

            read_individual = get_individual(ticket.id, new_test_doc3_uri);
            ok((read_individual['@'] == new_test_doc3_uri) == true);
        });

    test("#010 Individual of [v-s:Membership] store 3 and read 2 (ignore duplicate data)",
        function()
        {
            var ticket = get_admin_ticket();

            var memberOf = guid();
            var resources = guid();

            var res = addToGroup(ticket, memberOf, resources)
            var new_test_doc1 = res[0];
            var new_test_doc1_uri = new_test_doc1['@'];
            wait_module(subject_manager, res[1].op_id);

            var read_individual = get_individual(ticket.id, new_test_doc1_uri);
            ok(compare(new_test_doc1, read_individual));

            var new_test_doc2 = new_test_doc1;
            var new_test_doc2_uri = guid();
            new_test_doc2['@'] = new_test_doc2_uri;
            new_test_doc2['v-s:memberOf'] = newUri (guid());
            var res = put_individual(ticket.id, new_test_doc2);
            wait_module(subject_manager, res.op_id);

            read_individual = get_individual(ticket.id, new_test_doc2_uri);
            ok(compare(new_test_doc2, read_individual));

            var new_test_doc3 = new_test_doc2;
            var new_test_doc3_uri = guid();
            new_test_doc3['@'] = new_test_doc3_uri;
            new_test_doc3['v-s:memberOf'] = newUri (memberOf);
            try
            {
                var res = put_individual(ticket.id, new_test_doc3);
            }
            catch (err)
            {}
            wait_module(subject_manager, res.op_id);

            try
            {
                read_individual = get_individual(ticket.id, new_test_doc3_uri);
            }
            catch (e)
            {
                read_individual = {};
            }
            ok((read_individual['@'] == new_test_doc3_uri) == false);

        });

    test("#011 Individual of [v-s:NoMembership] store 3 and read 3 (this no membership)",
        function()
        {
            var ticket = get_admin_ticket();

            var memberOf = guid();
            var resources = guid();
            
            var new_test_doc1_uri = guid();
            var new_test_doc1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri ('v-s:ThisNoMembership'),
                'v-s:memberOf': newUri (memberOf),
                'v-s:resource': newUri (resources)
            };

            var res = put_individual(ticket.id, new_test_doc1);
            wait_module(subject_manager, res.op_id);

            var read_individual = get_individual(ticket.id, new_test_doc1_uri);
            ok(compare(new_test_doc1, read_individual));

            var new_test_doc2 = new_test_doc1;
            var new_test_doc2_uri = guid();
            new_test_doc2['@'] = new_test_doc2_uri;
            new_test_doc2['v-s:memberOf'] = newUri (guid());
            var res = put_individual(ticket.id, new_test_doc2);
            wait_module(subject_manager, res.op_id);

            read_individual = get_individual(ticket.id, new_test_doc2_uri);
            ok(compare(new_test_doc2, read_individual));

            var new_test_doc3 = new_test_doc2;
            var new_test_doc3_uri = guid();
            new_test_doc3['@'] = new_test_doc3_uri;
            new_test_doc3['v-s:memberOf'] = newUri (memberOf);
            var res = put_individual(ticket.id, new_test_doc3);
            wait_module(subject_manager, res.op_id);

            read_individual = get_individual(ticket.id, new_test_doc3_uri);
            ok((read_individual['@'] == new_test_doc3_uri) == true);
        });

    test(
        "#012 user1 store 3 individuals (one of the individuals contains an invalid field [author]), the user1 finds 2 individuals, and the user2 does not find anything.",
        function()
        {
            var ticket_user1 = get_user1_ticket();
            ok(ticket_user1.id.length > 0);

            var ticket_user2 = get_user2_ticket();
            ok(ticket_user2.id.length > 0);

            var new_test_doc1_uri_1 = "test12:" + guid();

            var test_data_uid = guid();
            var test_data = 'testdata ' + test_data_uid;

            var new_test_doc1 = {
                '@': new_test_doc1_uri_1,
                'rdf:type': newUri ('v-s:Document1'),
                'v-s:author': newUri ('td:ValeriyBushenev-Programmer1'),
                'v-s:test_field': newStr (test_data, 'NONE'),
                'v-s:test_fieldA': newUri ('BBB' + test_data_uid),
                'v-s:test_fieldB': newUri ('CCC' + test_data_uid)
            };

            // document content author != user1
            var new_test_doc1_uri_2 = "test12:" + guid();
            var new_test_doc2 = {
                '@': new_test_doc1_uri_2,
                'rdf:type': newUri ('v-s:Document1'),
                'v-s:author': newUri ('td:ValeriyBushenev-Programmer2'),
                'v-s:test_field': newUri (test_data)
            };

            var new_test_doc1_uri_3 = "test12:" + guid();
            var new_test_doc3 = {
                '@': new_test_doc1_uri_3,
                'rdf:type': newUri ('v-s:Document1'),
                'v-s:author': newUri ('td:ValeriyBushenev-Programmer1'),
                'v-s:test_field': newUri (test_data),
                'v-s:test_fieldA': newUri ('BBB' + test_data_uid)
            };

            var new_test_doc1_uri_4 = "test12:" + guid();
            var new_test_doc4 = {
                '@': new_test_doc1_uri_4,
                'rdf:type': newUri ('v-s:Document1'),
                'v-s:author': newUri ('td:ValeriyBushenev-Programmer1'),
                'v-s:test_field': newUri ('AAA' + test_data_uid),
                'v-s:test_fieldA': newUri ('BBB' + test_data_uid),
                'v-s:test_fieldB': newUri ('CCC' + test_data_uid)
            };

            var res = put_individual(ticket_user1.id, new_test_doc1, false);
            var res = put_individual(ticket_user1.id, new_test_doc2, false);
            var res = put_individual(ticket_user1.id, new_test_doc3, false);
            var res = put_individual(ticket_user1.id, new_test_doc4, false);

            wait_module(fulltext_indexer, res.op_id);
            wait_module(subject_manager, res.op_id);
            wait_module(acl_manager, res.op_id);
            wait_module(condition, res.op_id);

            var data = query(ticket_user1.id, test_data_uid, undefined, undefined, true);
            ok(compare(data.length, 2));

            data = query(ticket_user2.id, test_data_uid, undefined, undefined, true);
            ok(compare(data.length, 0));

            data = query(ticket_user1.id, "'v-s:test_field' == '" + test_data_uid + "'", undefined, undefined, true);
            ok(compare(data.length, 2));

            data = query(ticket_user1.id, "'v-s:test_field1' == '" + test_data_uid + "'", undefined, undefined, true);
            ok(compare(data.length, 0));

            data = query(ticket_user1.id, "'v-s:test_field1' == '" + test_data_uid + " t1'", undefined, undefined, true);
            ok(compare(data.length, 0));

            data = query(ticket_user1.id, "'v-s:test_field' == '" + test_data_uid + "' || 'v-s:test_field' == 'AAA" + test_data_uid + "'", undefined, undefined, true);
            ok(compare(data.length, 3));

            data = query(ticket_user1.id, "'v-s:test_fieldB' == 'CCC" + test_data_uid + "' && 'v-s:test_fieldA' == 'BBB" + test_data_uid + "'", undefined, undefined, true);
            ok(compare(data.length, 2));
        });

    test(
        "#013 user1 store 5 individuals, ft search use range ",
        function()
        {
            var ticket_user1 = get_user1_ticket();
            ok(ticket_user1.id.length > 0);

            var test_group_uid = guid();

            var new_test_doc1_uri = "test13:" + guid();
            var new_test_doc1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri ('v-s:Document1'),
                'v-s:author': newUri ('td:ValeriyBushenev-Programmer1'),
                'v-s:created': newDate(new Date()),
                'v-s:test_group': newUri (test_group_uid),
                'v-s:test_datetime0': newDate (new Date("2014-01-01")),
                'v-s:test_datetime1': newDate (new Date("2014-05-01"))
            };

            var new_test_doc2_uri = "test13:" + guid();
            var new_test_doc2 = {
                '@': new_test_doc2_uri,
                'rdf:type': newUri ('v-s:Document1'),
                'v-s:author': newUri ('td:ValeriyBushenev-Programmer1'),
                'v-s:created': newDate (new Date()),
                'v-s:test_group': newUri (test_group_uid),
                'v-s:test_datetime0': newDate (new Date("2014-01-02")),
                'v-s:test_datetime1': newDate (new Date("2014-05-01"))
            };

            var new_test_doc3_uri = "test13:" + guid();
            var new_test_doc3 = {
                '@': new_test_doc3_uri,
                'rdf:type': newUri ('v-s:Document1'),
                'v-s:author': newUri ('td:ValeriyBushenev-Programmer1'),
                'v-s:created': newDate (new Date()),
                'v-s:test_group': newUri (test_group_uid),
                'v-s:test_datetime0': newDate (new Date("2014-01-02")),
                'v-s:test_datetime1': newDate (new Date("2014-06-11"))
            };

            var new_test_doc4_uri = "test13:" + guid();
            var new_test_doc4 = {
                '@': new_test_doc4_uri,
                'rdf:type': newUri ('v-s:Document1'),
                'v-s:author': newUri ('td:ValeriyBushenev-Programmer1'),
                'v-s:created': newDate (new Date()),
                'v-s:test_group': newUri (test_group_uid),
                'v-s:test_datetime0': newDate (new Date("2014-01-04")),
                'v-s:test_datetime1': newDate (new Date("2014-06-12"))
            };

            var res = put_individual(ticket_user1.id, new_test_doc1, false);
            var res = put_individual(ticket_user1.id, new_test_doc2, false);
            var res = put_individual(ticket_user1.id, new_test_doc3, false);
            var res = put_individual(ticket_user1.id, new_test_doc4, false);

            wait_module(fulltext_indexer, res.op_id);
            wait_module(subject_manager, res.op_id);
            wait_module(acl_manager, res.op_id);
            wait_module(condition, res.op_id);

            var data = query(ticket_user1.id, test_group_uid, undefined, undefined, true);
            ok(compare(data.length, 4));

            data = query(ticket_user1.id, "'v-s:test_group' == '" + test_group_uid + "'", undefined, undefined, true);
            ok(compare(data.length, 4));

            data = query(ticket_user1.id,
                "'v-s:test_datetime0' == [2013-12-31T00:00:00, 2014-01-03T00:00:00] && 'v-s:test_group' == '" + test_group_uid + "'", undefined, undefined, true);
            ok(compare(data.length, 3));
            ok((data[0] == new_test_doc1_uri || data[1] == new_test_doc1_uri || data[2] == new_test_doc1_uri) &&
                (data[0] == new_test_doc2_uri || data[1] == new_test_doc2_uri || data[2] == new_test_doc2_uri) &&
                (data[0] == new_test_doc3_uri || data[1] == new_test_doc3_uri || data[2] == new_test_doc3_uri));

            data = query(ticket_user1.id,
                "'v-s:test_datetime1' == [2014-04-01T00:00:00, 2014-06-03T00:00:00] && 'v-s:test_datetime0' == [2013-12-31T00:00:00, 2014-01-03T00:00:00] && 'v-s:test_group' == '" + test_group_uid + "'", undefined, undefined, true);
            ok(compare(data.length, 2));
            ok((data[0] == new_test_doc1_uri || data[1] == new_test_doc1_uri) && (data[0] == new_test_doc2_uri || data[1] == new_test_doc2_uri));
        });


    test(
        "#014 Individual store, add_to_individual, set_in_individual test",
        function()
        {
            var ticket_user1 = get_user1_ticket();
            ok(ticket_user1.id.length > 0);

            var new_test_doc1_uri = "test14:" + guid();
            var new_test_doc1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri ('v-s:Document1'),
                'v-s:author': newUri ('td:ValeriyBushenev-Programmer1'),
                'v-s:test_field': newStr ('test data', 'EN')
            };

            var res = put_individual(ticket_user1.id, new_test_doc1);
            wait_module(condition, res.op_id);
            wait_module(acl_manager, res.op_id);

            var read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);
            ok(compare(new_test_doc1, read_individual));

            var new_test_add1 = {
                '@': new_test_doc1_uri,
                'v-s:author': [
                {
                    data: 'td:ValeriyBushenev-Programmer2',
                    type: _Uri
                },
                {
                    data: 'td:test-q',
                    type: _Uri
                }]
            };

            add_to_individual(ticket_user1.id, new_test_add1);
            wait_module(condition, res.op_id);
            wait_module(acl_manager, res.op_id);

            var new_test_doc1_add1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri ('v-s:Document1'),
                'v-s:author': [
                {
                    data: 'td:ValeriyBushenev-Programmer1',
                    type: _Uri
                },
                {
                    data: 'td:ValeriyBushenev-Programmer2',
                    type: _Uri
                },
                {
                    data: 'td:test-q',
                    type: _Uri
                }],
                'v-s:test_field': newStr ('test data','EN')
            };

            read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);
            ok(compare(new_test_doc1_add1, read_individual));

            var new_test_set1 = {
                '@': new_test_doc1_uri,
                'v-s:author': newUri ('td:test-e')
            };

            set_in_individual(ticket_user1.id, new_test_set1);
            wait_module(condition, res.op_id);
            wait_module(acl_manager, res.op_id);

            var new_test_doc1_set1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri ('v-s:Document1'),
                'v-s:author': newUri ('td:test-e'),
                'v-s:test_field': newStr ('test data','EN')
            };

            read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);
            ok(compare(new_test_doc1_set1, read_individual));
        });
}
