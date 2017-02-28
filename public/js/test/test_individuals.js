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

function generate_test_document1(ticket)
{
    var new_test_doc1_uri = guid();
    var new_test_doc1 = {
        '@': new_test_doc1_uri,
        'rdf:type': newUri('v-s:test-data-types'),
        'v-s:test_integer': newInt(9223372036854775295),
        'v-s:test_negative_integer': newInt(-144365435),
        'v-s:test_decimal': newDecimal(12.12345678912345),
        'v-s:test_negative_decimal': newDecimal(-54.89764),
        'v-s:test_decimal2': newDecimal(0.7),
        'v-s:test_decimal3': newDecimal(764.3),
        'v-s:test_decimal4': newDecimal(90.8),
        'v-s:test_decimal5': newDecimal(7.6),
        'v-s:test_decimal6': newDecimal(0.07),
        'v-s:test_decimal6_1': newDecimal(-0.07),
        'v-s:test_decimal7': newDecimal(0.007),
        'v-s:created': newDate(new Date()),
        'v-s:test_datetime0': newDate(new Date("2014-01-02")),
        'v-s:test_datetime1': newDate(new Date("2014-01-02T20:00")),
        'v-s:test_datetime2': newDate(new Date("2014-01-02T20:10:24")),
        'v-s:test_datetime3': newDate(new Date("2014-01-02T20:10:24.768")),
        'v-s:test_datetime4': newDate(new Date("1960-01-02")),
        'v-s:canUpdate': newBool(true),
        'v-s:permissionSubject': newUri('individual_' + guid()),
        'v-s:author': newUri(ticket.user_uri)
    };
    return new_test_doc1;
}

function create_test_document1(ticket)
{
    var new_test_doc1 = generate_test_document1(ticket)
    var res = put_individual(ticket.id, new_test_doc1);
    wait_module(subject_manager, res.op_id);
    wait_module(acl_manager, res.op_id);
    return new_test_doc1;
}

function test_success_read(ticket, read_indv_uri, ethalon_indv, reopen)
{
    var read_individual;

    if (!reopen)
        reopen = false;

    try
    {
        read_individual = get_individual(ticket.id, read_indv_uri, reopen);
    }
    catch (e)
    {
        read_individual = {};
    }

    var res = compare(ethalon_indv, read_individual);
    ok(res == true);

    return res == true;
}

function test_fail_read(ticket, read_indv_uri, ethalon_indv, reopen)
{
    var read_individual;

    if (!reopen)
        reopen = false;

    try
    {
        read_individual = get_individual(ticket.id, read_indv_uri, true);
    }
    catch (e)
    {
        read_individual = {};
    }

    var res = compare(ethalon_indv, read_individual);
    ok(res == false);

    return res == false;
}

function check_rights_success(ticket, uri, expected_rights)
{
    var res = check_rights(ticket, uri, expected_rights);
    return ok(res === true);
}

function check_rights_fail(ticket, uri, expected_rights)
{
    var res = check_rights(ticket, uri, expected_rights);
    return ok(res === false);
}

function check_rights(ticket, uri, expected_rights)
{
    var rights = get_rights(ticket, uri);

    var result = true;

    for (var i = 0; i < expected_rights.length; i++)
    {
        var expected = expected_rights[i];
        if (expected === can_create)
        {
            result = result && ("v-s:canCreate" in rights);
        }
        else if (expected === can_read)
        {
            result = result && ("v-s:canRead" in rights);
        }
        else if (expected === can_update)
        {
            result = result && ("v-s:canUpdate" in rights);
        }
        else if (expected === can_delete)
        {
            result = result && ("v-s:canDelete" in rights);
        }
    }

    return result;
}

var i = 0;

for (i = 0; i < 1; i++)
{
    test(
        "#001 Login",
        function()
        {
            var ticket = get_user1_ticket();
            ok(ticket.id.length > 0);
        });

    test(
        "#002 Get individual 'owl:'",
        function()
        {
            var ticket = get_user1_ticket();
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
            var ticket = get_user1_ticket();
            var data = query(ticket.id, "owl:").result;
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
                'rdf:type': newUri('rdfs:Resource'),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:test_field': newStr('test data', 'EN')
            };

            var res = put_individual(ticket_user1.id, new_test_doc1);
            wait_module(condition, res.op_id);
            wait_module(acl_manager, res.op_id);

            test_success_read(ticket_user1, new_test_doc1['@'], new_test_doc1);
            test_fail_read(ticket_user2, new_test_doc1['@'], new_test_doc1);

            res = remove_individual (ticket_user1.id, new_test_doc1['@']);
            wait_module(condition, res.op_id);
            //wait_module(acl_manager, res.op_id);

            test_fail_read(ticket_user1, new_test_doc1['@'], new_test_doc1);
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
                'rdf:type': newUri('rdfs:Resource'),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:test_field': newStr('test data', 'NONE')
            };

            var res = put_individual(ticket_user1.id, new_test_doc1);
            wait_module(condition, res.op_id);
            wait_module(acl_manager, res.op_id);

            test_fail_read(ticket_user2, new_test_doc1_uri, new_test_doc1);
            test_success_read(ticket_user1, new_test_doc1_uri, new_test_doc1);
            var read_individual;

            var res = addRight(ticket_user1.id, [can_read], ticket_user2.user_uri, new_test_doc1_uri);
            var new_permission = res[0];
            wait_module(acl_manager, res[1].op_id);

            test_success_read(ticket_user2, new_test_doc1_uri, new_test_doc1, true);

            new_permission["@"] = "_";
            delete new_permission["v-s:permissionObject"];
            delete new_permission["v-s:permissionSubject"];

            var right1 = get_rights(ticket_user1.id, new_test_doc1_uri);
            var right2 = get_rights(ticket_user2.id, new_test_doc1_uri);

            ok(compare(new_permission, right2));

            new_permission['v-s:canUpdate'] = newBool(true);
            new_permission['v-s:canDelete'] = newBool(true);
            new_permission['v-s:canCreate'] = newBool(true);

            ok(compare(new_permission, right1));
            test_success_read(ticket_user2, new_test_doc1_uri, new_test_doc1);

            res = addRight(ticket_user1.id, [cant_read], ticket_user2.user_uri, new_test_doc1_uri);
            res = addRight(ticket_user1.id, [can_read], ticket_user2.user_uri, new_test_doc1_uri);
            wait_module(acl_manager, res[1].op_id);

            test_fail_read(ticket_user2, new_test_doc1_uri, new_test_doc1);

            try
            {
                // test UPDATE rights
                res = put_individual(ticket_user2.id, new_test_doc1);
                ok (false);
            }
            catch (e)
            {
                ok (true);
            }
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
                'rdf:type': newUri('rdfs:Resource'),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:test_field': newStr('test data', 'NONE')
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
        var ticket = get_user1_ticket();

        var new_test_doc1 = create_test_document1(ticket);

        var read_individual = get_individual(ticket.id, new_test_doc1['@']);
        ok(compare(new_test_doc1, read_individual));
    });

    test("#008 test [v-s:PermissionStatement]: user1 store file, user2 not read file, add right for user2, add cant read right for user2",
        function()
        {
            var ticket1 = get_user1_ticket();
            var ticket2 = get_user2_ticket();

            var res;
            var new_test_doc1 = create_test_document1(ticket1);
            res = test_success_read(ticket1, new_test_doc1['@'], new_test_doc1);
            res = test_fail_read(ticket2, new_test_doc1['@'], new_test_doc1);

            res = addRight(ticket1.id, [can_read], ticket2.user_uri, new_test_doc1['@']);
            var op_id = res[1].op_id;
            wait_module(acl_manager, res[1].op_id);

            res = test_success_read(ticket2, new_test_doc1['@'], new_test_doc1, true);

            res = addRight(ticket1.id, [cant_read], ticket2.user_uri, new_test_doc1['@']);
            var op_id = res[1].op_id;
            wait_module(acl_manager, res[1].op_id);

            res = test_fail_read(ticket2, new_test_doc1['@'], new_test_doc1, true);
        });

    test(
        "#009 Individual of [v-s:NoPermissionStatement] store 3 and read 3",
        function()
        {
            var ticket = get_user1_ticket();

            var permissionSubject = guid();
            var permissionObject = guid();

            var new_test_doc1_uri = guid();
            var new_test_doc1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri('v-s:NoPermissionStatement'),
                'v-s:canDelete': newBool(true),
                'v-s:canRead': newBool(true),
                'v-s:canUpdate': newBool(true),
                'v-s:permissionObject': newUri(permissionObject),
                'v-s:permissionSubject': newUri(permissionSubject),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1')
            };

            var res = put_individual(ticket.id, new_test_doc1);
            wait_module(subject_manager, res.op_id);
            wait_module(acl_manager, res.op_id);

            var read_individual = get_individual(ticket.id, new_test_doc1_uri);
            ok(compare(new_test_doc1, read_individual));

            var new_test_doc2 = new_test_doc1;
            var new_test_doc2_uri = guid();
            new_test_doc2['@'] = new_test_doc2_uri;
            new_test_doc2['v-s:canRead'] = newBool(false);
            var res = put_individual(ticket.id, new_test_doc2);
            wait_module(subject_manager, res.op_id);
            wait_module(acl_manager, res.op_id);

            read_individual = get_individual(ticket.id, new_test_doc2_uri);
            ok(compare(new_test_doc2, read_individual));

            var new_test_doc3 = new_test_doc2;
            var new_test_doc3_uri = guid();
            new_test_doc3['@'] = new_test_doc3_uri;
            new_test_doc3['v-s:canRead'] = newBool(true);
            var res = put_individual(ticket.id, new_test_doc3);
            wait_module(subject_manager, res.op_id);
            wait_module(acl_manager, res.op_id);

            read_individual = get_individual(ticket.id, new_test_doc3_uri);
            ok((read_individual['@'] == new_test_doc3_uri) == true);
        });

    test("#010 Individual of [v-s:Membership]",
        function()
        {
            var ticket1 = get_user1_ticket();
            var ticket2 = get_user2_ticket();

            var res;
            var new_test_doc1 = create_test_document1(ticket1);
            res = test_success_read(ticket1, new_test_doc1['@'], new_test_doc1);
            res = test_fail_read(ticket2, new_test_doc1['@'], new_test_doc1);

            var doc_group = 'g:doc_group_' + guid();
            var user_group = 'g:user_group_' + guid();

            res = addToGroup(ticket1, doc_group, new_test_doc1['@']);
            res = addToGroup(ticket1, user_group, ticket2.user_uri);

            var membersip1 = res[0];

            res = addRight(ticket1.id, [can_read], user_group, doc_group);
            var op_id = res[1].op_id;
            wait_module(acl_manager, res[1].op_id);

            res = test_success_read(ticket2, new_test_doc1['@'], new_test_doc1, true);

            res = removeFromGroup(ticket1, user_group, ticket2.user_uri);
            wait_module(acl_manager, res[1].op_id);

            test_fail_read(ticket2, new_test_doc1['@'], new_test_doc1, true);
        });

    test("#011 Individual of [v-s:NoMembership] store 3 and read 3 (this no membership)",
        function()
        {
            var ticket = get_user1_ticket();

            var memberOf = guid();
            var resources = guid();

            var new_test_doc1_uri = guid();
            var new_test_doc1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri('v-s:ThisNoMembership'),
                'v-s:memberOf': newUri(memberOf),
                'v-s:resource': newUri(resources),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1')
            };

            var res = put_individual(ticket.id, new_test_doc1);
            wait_module(acl_manager, res.op_id);
            wait_module(subject_manager, res.op_id);

            var read_individual = get_individual(ticket.id, new_test_doc1_uri);
            ok(compare(new_test_doc1, read_individual));

            var new_test_doc2 = new_test_doc1;
            var new_test_doc2_uri = guid();
            new_test_doc2['@'] = new_test_doc2_uri;
            new_test_doc2['v-s:memberOf'] = newUri(guid());
            var res = put_individual(ticket.id, new_test_doc2);
            wait_module(acl_manager, res.op_id);
            wait_module(subject_manager, res.op_id);

            read_individual = get_individual(ticket.id, new_test_doc2_uri);
            ok(compare(new_test_doc2, read_individual));

            var new_test_doc3 = new_test_doc2;
            var new_test_doc3_uri = guid();
            new_test_doc3['@'] = new_test_doc3_uri;
            new_test_doc3['v-s:memberOf'] = newUri(memberOf);
            var res = put_individual(ticket.id, new_test_doc3);
            wait_module(subject_manager, res.op_id);
            wait_module(acl_manager, res.op_id);

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
                'rdf:type': newUri('rdfs:Resource'),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:test_field': newStr(test_data, 'NONE'),
                'v-s:test_fieldA': newUri('BBB' + test_data_uid),
                'v-s:test_fieldB': newUri('CCC' + test_data_uid)
            };

            // document content author != user1
            var new_test_doc1_uri_2 = "test12:" + guid();
            var new_test_doc2 = {
                '@': new_test_doc1_uri_2,
                'rdf:type': newUri('rdfs:Resource'),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer2'),
                'v-s:test_field': newUri(test_data)
            };

            var new_test_doc1_uri_3 = "test12:" + guid();
            var new_test_doc3 = {
                '@': new_test_doc1_uri_3,
                'rdf:type': newUri('rdfs:Resource'),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:test_field': newUri(test_data),
                'v-s:test_fieldA': newUri('BBB' + test_data_uid)
            };

            var new_test_doc1_uri_4 = "test12:" + guid();
            var new_test_doc4 = {
                '@': new_test_doc1_uri_4,
                'rdf:type': newUri('rdfs:Resource'),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:test_field': newUri('AAA' + test_data_uid),
                'v-s:test_fieldA': newUri('BBB' + test_data_uid),
                'v-s:test_fieldB': newUri('CCC' + test_data_uid)
            };

            var res = put_individual(ticket_user1.id, new_test_doc1, false);
            var res = put_individual(ticket_user1.id, new_test_doc2, false);
            var res = put_individual(ticket_user1.id, new_test_doc3, false);
            var res = put_individual(ticket_user1.id, new_test_doc4, false);

            flush (fulltext_indexer, res.op_id);

            wait_module(fulltext_indexer, res.op_id);
            wait_module(subject_manager, res.op_id);
            //wait_module(acl_manager, res.op_id);
            //wait_module(condition, res.op_id);

            var data = query(ticket_user1.id, test_data_uid, undefined, undefined, true).result;
            ok(compare(data.length, 2));

            data = query(ticket_user2.id, test_data_uid, undefined, undefined, true).result;
            ok(compare(data.length, 0));

            data = query(ticket_user1.id, "'v-s:test_field' === '" + test_data_uid + "'", undefined, undefined, true).result;
            ok(compare(data.length, 2));

            data = query(ticket_user1.id, "'v-s:test_field1' === '" + test_data_uid + "'", undefined, undefined, true).result;
            ok(compare(data.length, 0));

            data = query(ticket_user1.id, "'v-s:test_field1' === '" + test_data_uid + " t1'", undefined, undefined, true).result;
            ok(compare(data.length, 0));

            data = query(ticket_user1.id, "'v-s:test_field' === '" + test_data_uid + "' || 'v-s:test_field' === 'AAA" + test_data_uid + "'", undefined, undefined, true).result;
            ok(compare(data.length, 3));

            data = query(ticket_user1.id, "'v-s:test_fieldB' === 'CCC" + test_data_uid + "' && 'v-s:test_fieldA' === 'BBB" + test_data_uid + "'", undefined, undefined, true).result;
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
                'rdf:type': newUri('rdfs:Resource'),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:created': newDate(new Date()),
                'v-s:test_group': newUri(test_group_uid),
                'v-s:test_datetime0': newDate(new Date("2014-01-01")),
                'v-s:test_datetime1': newDate(new Date("2014-05-01"))
            };

            var new_test_doc2_uri = "test13:" + guid();
            var new_test_doc2 = {
                '@': new_test_doc2_uri,
                'rdf:type': newUri('rdfs:Resource'),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:created': newDate(new Date()),
                'v-s:test_group': newUri(test_group_uid),
                'v-s:test_datetime0': newDate(new Date("2014-01-02")),
                'v-s:test_datetime1': newDate(new Date("2014-05-01"))
            };

            var new_test_doc3_uri = "test13:" + guid();
            var new_test_doc3 = {
                '@': new_test_doc3_uri,
                'rdf:type': newUri('rdfs:Resource'),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:created': newDate(new Date()),
                'v-s:test_group': newUri(test_group_uid),
                'v-s:test_datetime0': newDate(new Date("2014-01-02")),
                'v-s:test_datetime1': newDate(new Date("2014-06-11"))
            };

            var new_test_doc4_uri = "test13:" + guid();
            var new_test_doc4 = {
                '@': new_test_doc4_uri,
                'rdf:type': newUri('rdfs:Resource'),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:created': newDate(new Date()),
                'v-s:test_group': newUri(test_group_uid),
                'v-s:test_datetime0': newDate(new Date("2014-01-04")),
                'v-s:test_datetime1': newDate(new Date("2014-06-12"))
            };

            var res = put_individual(ticket_user1.id, new_test_doc1, false);
            var res = put_individual(ticket_user1.id, new_test_doc2, false);
            var res = put_individual(ticket_user1.id, new_test_doc3, false);
            var res = put_individual(ticket_user1.id, new_test_doc4, false);

            wait_module(fulltext_indexer, res.op_id);
            wait_module(subject_manager, res.op_id);
            //wait_module(acl_manager, res.op_id);
            //wait_module(condition, res.op_id);

            var data = query(ticket_user1.id, test_group_uid, undefined, undefined, true).result;
            ok(compare(data.length, 4));

            data = query(ticket_user1.id, "'v-s:test_group' === '" + test_group_uid + "'", undefined, undefined, true).result;
            ok(compare(data.length, 4));

            data = query(ticket_user1.id,
                "'v-s:test_datetime0' === [2013-12-31T00:00:00, 2014-01-03T00:00:00] && 'v-s:test_group' === '" + test_group_uid + "'", undefined, undefined, true).result;
            ok(compare(data.length, 3));
            ok((data[0] == new_test_doc1_uri || data[1] == new_test_doc1_uri || data[2] == new_test_doc1_uri) &&
                (data[0] == new_test_doc2_uri || data[1] == new_test_doc2_uri || data[2] == new_test_doc2_uri) &&
                (data[0] == new_test_doc3_uri || data[1] == new_test_doc3_uri || data[2] == new_test_doc3_uri));

            data = query(ticket_user1.id,
                "'v-s:test_datetime1' === [2014-04-01T00:00:00, 2014-06-03T00:00:00] && 'v-s:test_datetime0' === [2013-12-31T00:00:00, 2014-01-03T00:00:00] && 'v-s:test_group' === '" + test_group_uid + "'", undefined, undefined, true).result;
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
                'rdf:type': newUri('rdfs:Resource'),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:test_field': newStr('test data', 'EN')
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
                'rdf:type': newUri('rdfs:Resource'),
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
                'v-s:test_field': newStr('test data', 'EN')
            };

            read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);
            ok(compare(new_test_doc1_add1, read_individual));

            var new_test_set1 = {
                '@': new_test_doc1_uri,
                'v-s:author': newUri('td:test-e')
            };

            set_in_individual(ticket_user1.id, new_test_set1);
            wait_module(condition, res.op_id);
            wait_module(acl_manager, res.op_id);

            var new_test_doc1_set1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri('rdfs:Resource'),
                'v-s:author': newUri('td:test-e'),
                'v-s:test_field': newStr('test data', 'EN')
            };

            read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);
            ok(compare(new_test_doc1_set1, read_individual));
        });

    test("#015 Document as a group",
        function()
        {
            var ticket1 = get_user1_ticket();
            var ticket2 = get_user2_ticket();

            var res;
            var doc1 = create_test_document1(ticket1);
            var doc2 = create_test_document1(ticket1);

            res = test_success_read(ticket1, doc1['@'], doc1);
            res = test_fail_read(ticket2, doc1['@'], doc1);

            res = test_success_read(ticket1, doc2['@'], doc2);
            res = test_fail_read(ticket2, doc2['@'], doc2);

            res = addToGroup(ticket1, doc1['@'], doc2['@']);
            res = addRight(ticket1.id, [can_read], ticket2.user_uri, doc1['@']);

            wait_module(acl_manager, res[1].op_id);

            res = test_success_read(ticket2, doc1['@'], doc1, true);
            res = test_success_read(ticket2, doc2['@'], doc2, true);

            res = removeFromGroup(ticket1, doc1['@'], doc2['@']);

            wait_module(acl_manager, res[1].op_id);

            res = test_success_read(ticket2, doc1['@'], doc1, true);
            res = test_fail_read(ticket2, doc2['@'], doc2, true);
        });

    test("#016 Nested groups",
        function()
        {
            var ticket1 = get_user1_ticket();
            var ticket2 = get_user2_ticket();

            var res;
            var doc1 = create_test_document1(ticket1);
            var doc2 = create_test_document1(ticket1);
            var doc_group1_uri = 'g:doc_group_' + guid();

            res = test_success_read(ticket1, doc1['@'], doc1);
            res = test_fail_read(ticket2, doc1['@'], doc1);

            res = test_success_read(ticket1, doc2['@'], doc2);
            res = test_fail_read(ticket2, doc2['@'], doc2);

            res = addToGroup(ticket1, doc1['@'], doc2['@']);
            res = addToGroup(ticket1, doc_group1_uri, doc1['@']);

            res = addRight(ticket1.id, [can_read], ticket2.user_uri, doc_group1_uri);

            var op_id = res[1].op_id;
            wait_module(acl_manager, res[1].op_id);

            res = test_success_read(ticket2, doc1['@'], doc1, true);
            res = test_success_read(ticket2, doc2['@'], doc2, true);
        });

    test("#017 Nested groups with restrictions",
        function()
        {
            var ticket1 = get_user1_ticket();
            var ticket2 = get_user2_ticket();

            var res;
            var doc1 = create_test_document1(ticket1);
            var doc2 = create_test_document1(ticket1);
            var doc3 = create_test_document1(ticket1);
            var doc_group1_uri = 'g:doc_group_' + guid();
            var doc_group2_uri = 'g:doc_group_' + guid();
            var doc_group3_uri = 'g:doc_group_' + guid();

            res = test_success_read(ticket1, doc1['@'], doc1);
            res = test_fail_read(ticket2, doc1['@'], doc1);

            res = test_success_read(ticket1, doc2['@'], doc2);
            res = test_fail_read(ticket2, doc2['@'], doc2);

            res = test_success_read(ticket1, doc3['@'], doc3);
            res = test_fail_read(ticket2, doc3['@'], doc3);

            res = addToGroup(ticket1, doc1['@'], doc2['@']);
            res = addToGroup(ticket1, doc1['@'], doc3['@'], [can_read]);
            res = addToGroup(ticket1, doc_group1_uri, doc1['@']);
            res = addToGroup(ticket1, doc_group2_uri, doc1['@']);
            res = addToGroup(ticket1, doc_group3_uri, doc_group1_uri);
            res = addToGroup(ticket1, doc_group3_uri, doc_group2_uri);

            res = addRight(ticket1.id, [can_read], ticket2.user_uri, doc_group3_uri);
            var op_id = res[1].op_id;
            wait_module(acl_manager, res[1].op_id);

            res = addRight(ticket1.id, [can_update], ticket2.user_uri, doc_group2_uri);
            var op_id = res[1].op_id;
            wait_module(acl_manager, res[1].op_id);

            res = addRight(ticket1.id, [can_delete], ticket2.user_uri, doc_group1_uri);
            var op_id = res[1].op_id;
            wait_module(acl_manager, res[1].op_id);

            check_rights_success(ticket2.id, doc1['@'], [can_read, can_update, can_delete]);
            check_rights_success(ticket2.id, doc3['@'], [can_read]);
            check_rights_fail(ticket2.id, doc3['@'], [can_update]);
            check_rights_fail(ticket2.id, doc3['@'], [can_delete]);

        });

    test("#018 Search with cursor",
        function()
        {
          var user = authenticate("bushenevvt", "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3");
          var admin = authenticate("karpovrt", "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3");

          var meeting_template = '{\
            "@": "d:QueryTestResource_$i", \
            "rdf:type": [{ "type": "Uri", "data": "rdfs:Resource" }], \
            "v-s:creator": [{ "type": "Uri", "data": "$creator" }], \
            "rdfs:label": [{ "type": "String", "data": "$i", "lang": "NONE" }] \
          }';

          createMeetings(user, 1, 5);
          createMeetings(admin, 6, 3);
          createMeetings(user, 9, 3);
          createMeetings(admin, 12, 9);

          var q = "'rdf:type'==='rdfs:Resource' && '@'=='d:QueryTestResource*'";
          var s = "'rdfs:label' asc"

          var params_admin1 = {
            ticket: admin.id,
            query: q,
            sort: s,
            top: 3,
            from: 0
          };
          var results_admin1 = query(params_admin1);
          //console.log("params_admin1", params_admin1, "results_admin1", results_admin1);
          ok(results_admin1.count === 3 && results_admin1.cursor === 3 && results_admin1.processed === 3);

          var params_admin2 = {
            ticket: admin.id,
            query: q,
            sort: s,
            top: 10,
            from: 10
          };
          var results_admin2 = query(params_admin2);
          //console.log("params_admin2", params_admin2, "results_admin2", results_admin2);
          ok(results_admin2.count === 10 && results_admin2.cursor === 20 && results_admin2.processed === 10);

          var params_user1 = {
            ticket: user.id,
            query: q,
            sort: s,
            top: 6,
            from: 0
          };
          var results_user1 = query(params_user1);
          //console.log("params_user1", params_user1, "results_user1", results_user1);
          ok(results_user1.count === 6 && results_user1.cursor === 9 && results_user1.processed === 9);

          var params_user2 = {
            ticket: user.id,
            query: q,
            sort: s,
            top: 10,
            limit: 10,
            from: 3
          };
          var results_user2 = query(params_user2);
          //console.log("params_user2", params_user2, "results_user2", results_user2);
          ok(results_user2.count === 5 && results_user2.cursor === 13 && results_user2.processed === 10);

          function createMeetings(creator, start, count) {
            for (var i = start; i < start + count; i++) {
              var meeting = JSON.parse( meeting_template.replace(/\$i/g, i.toString().length === 2 ? i : "0" + i ).replace(/\$creator/g, creator.user_uri) );
              var res = put_individual(creator.id, meeting);
              wait_module(fulltext_indexer, res.op_id);
            }
          }

        });
}
