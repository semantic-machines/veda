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
    var new_test_doc1_uri = genUri();
    var new_test_doc1 = {
        '@': new_test_doc1_uri,
        'rdf:type': newUri('rdfs:Resource1'),
        'v-s:test_integer_32': newInt(922337203),
        'v-s:test_integer_64': newInt(9223372036854775295),
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
        'v-s:test_decimal8': newDecimal(1),
        'v-s:test_decimal9': newDecimal(5.0),
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

function generate_test_document2(ticket)
{
    var new_test_doc2_uri = genUri();
    var new_test_doc2 = {
        '@': new_test_doc2_uri,
        'rdf:type': newUri('rdfs:Resource1'),
        'v-s:test_integer_32': [newInt(922337203)[0], newInt(456403)[0]],
        'v-s:test_negative_integer': newInt(-144365435),
        'v-s:test_decimal': newDecimal(12.12345678912345),
        'v-s:test_negative_decimal': newDecimal(-54.89764),
        'v-s:test_decimal2': [newDecimal(0.7)[0], newDecimal(0.4)[0]],
        'v-s:test_decimal3': newDecimal(764.3),
        'v-s:test_decimal4': newDecimal(90.8),
        'v-s:test_decimal5': newDecimal(7.6),
        'v-s:test_decimal6': newDecimal(0.07),
        'v-s:test_decimal6_1': newDecimal(-0.07),
        'v-s:test_decimal7': newDecimal(0.007),
        'v-s:test_decimal8': newDecimal(1),
        'v-s:test_decimal9': newDecimal(5.0),
        'v-s:created': newDate(new Date()),
        'v-s:test_datetime0': newDate(new Date("2014-01-02")),
        'v-s:test_datetime1': newDate(new Date("2014-01-02T20:00")),
        'v-s:test_datetime2': newDate(new Date("2014-01-02T20:10:24")),
        'v-s:test_datetime3': newDate(new Date("2014-01-02T20:10:24.768")),
        'v-s:test_datetime4': newDate(new Date("1960-01-02")),
        'v-s:canUpdate': newBool(true),
        'v-s:permissionSubject': [newUri('individual_' + guid())[0], newUri('individual_' + guid())[0]],
	'rdfs:label': [newStr ("Русский", "RU")[0], newStr ("English", "EN")[0]],
        'v-s:author': newUri(ticket.user_uri)
    };

    return new_test_doc2;
}

function create_test_document1(ticket, prefix)
{
    var new_test_doc1 = generate_test_document1(ticket)

    if (prefix)
  new_test_doc1['@'] = prefix + new_test_doc1['@']

    var res = put_individual(ticket.id, new_test_doc1);
    //wait_module(m_subject, res.op_id);
    wait_module(m_acl, res.op_id);
    wait_module(m_scripts, res.op_id);
    return new_test_doc1;
}

function create_test_document2(ticket, prefix)
{
    var new_test_doc2 = generate_test_document2(ticket)

    if (prefix)
	new_test_doc2['@'] = prefix + new_test_doc2['@']

    var res = put_individual(ticket.id, new_test_doc2);
    //wait_module(m_subject, res.op_id);
    wait_module(m_acl, res.op_id);
    wait_module(m_scripts, res.op_id);
    return new_test_doc2;
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

            //#1
            ok(ticket.id.length > 0);
        });

    test(
        "#002 Get individual 'owl:'",
        function()
        {
            var ticket = get_user1_ticket();
            var res = get_individual(ticket.id, "owl:");

            //#1
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

            //#2
            ok(ee != null);

            //#3
            ok(ee.data == "Словарь OWL 2 Schema (OWL 2)");
        });

    test(
        "#003 Query '@' == 'owl:' ++ Get individual 'owl:'",
        function()
        {
            var ticket = get_user1_ticket();
            var data = query(ticket.id, "owl:").result;

            //#1
            ok(data.indexOf("owl:") >= 0);
        });

    test(
        "#030 search form test",
        function()
        {
            var res;
            var ticket_user1 = get_user1_ticket();

            var test_group_uid = "test30:" + guid();

            var new_test_doc1_uri = "test30:" + guid();
            var label1 = "test30.1:" + guid();
            var comment = "comment30:" + guid();
            var new_test_doc1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri('rdfs:Resource'),
                'rdfs:label': newUri(label1),
                'rdfs:comment': newUri(comment),
                'v-s:test_group': newUri(test_group_uid),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:created': newDate(new Date())
            };

            var new_test_doc2_uri = "test30:" + guid();
            var label2 = "test30.2:" + guid();
            var new_test_doc2 = {
                '@': new_test_doc2_uri,
                'rdf:type': newUri('rdfs:Resource'),
                'rdfs:label': newUri(label2),
                'v-s:test_group': newUri(test_group_uid),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:created': newDate(new Date())
            };

            var new_test_doc3_uri = "test30.1:" + guid();
            var new_test_doc3 = {
                '@': new_test_doc3_uri,
                'rdf:type': newUri('rdfs:Resource'),
                'rdfs:label': newUri(label1),
                'v-s:test_group': newUri(test_group_uid),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:created': newDate(new Date())
            };

            var new_test_doc4_uri = "test30.1:" + guid();
            var new_test_doc4 = {
                '@': new_test_doc4_uri,
                'rdf:type': newUri('rdfs:Resource'),
                'rdfs:label': newUri(label2),
                'v-s:test_group': newUri(test_group_uid),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:created': newDate(new Date())
            };

            var new_test_doc5_uri = "test30.2:" + guid();
            var comment2 = "comm1" + guid();
            var new_test_doc5 = {
                '@': new_test_doc5_uri,
                'rdf:type': newUri('rdfs:Resource'),
                'rdfs:label': newUri(label1),
                'rdfs:comment' : newUri(comment2),
                'v-s:test_group': newUri(test_group_uid),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:created': newDate(new Date())
            };

            var new_test_doc6_uri = "test30.2:" + guid();
            var new_test_doc6 = {
                '@': new_test_doc6_uri,
                'rdf:type': newUri('rdfs:Resource'),
                'v-s:test_group': newUri(test_group_uid),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:created': newDate(new Date())
            };

        console.log("@1");

            res = put_individual(ticket_user1.id, new_test_doc1, false);
            res = put_individual(ticket_user1.id, new_test_doc2, false);
            res = put_individual(ticket_user1.id, new_test_doc3, false);
            res = put_individual(ticket_user1.id, new_test_doc4, false);
            res = put_individual(ticket_user1.id, new_test_doc5, false);
            res = put_individual(ticket_user1.id, new_test_doc6, false);

        console.log("@2");

            flush (m_fulltext_indexer, res.op_id);

        console.log("@3");

            wait_module(m_fulltext_indexer, res.op_id);
            wait_module(m_subject, res.op_id);

        console.log("@4");

            var data = query(ticket_user1.id, "'*' == 'test30.1*' && 'v-s:test_group' === '" + test_group_uid + "'" , undefined, undefined, true).result;

        console.log("@4.1");
            //#2
            ok(compare(data.length, 3));

        console.log("@5");

            var data = query(ticket_user1.id, test_group_uid, undefined, undefined, true).result;

        console.log("@5.0");
            //#1
            ok(compare(data.length, 6));

        console.log("@5.1");

            var data = query(ticket_user1.id, "'@' == 'test30.1*' && 'v-s:test_group' === '" + test_group_uid + "'" , undefined, undefined, true).result;

            //#3
            ok(compare(data.length, 2));

        console.log("@5.2");

            var data = query(ticket_user1.id, "('@' == 'test30.1*' || '@' == 'test30.2*') && 'v-s:test_group' === '" + test_group_uid + "'" , undefined, undefined, true).result;

            //#4
            ok(compare(data.length, 4));

        console.log("@5.3");

            var data = query(ticket_user1.id, "'@' == 'test30*' && 'v-s:test_group' === '" + test_group_uid + "'" , undefined, undefined, true).result;

            //#5
            ok(compare(data.length, 6));

        console.log("@5.4");

            var data = query(ticket_user1.id, "'rdfs:label.isExists' == 'true' && 'v-s:test_group' === '" + test_group_uid + "'" , undefined, undefined, true).result;

            //#6
            ok(compare(data.length, 5));

        console.log("@5.5");

            var data = query(ticket_user1.id, "'rdfs:comment' == 'comment*' && 'v-s:test_group' === '" + test_group_uid + "'" , undefined, undefined, true).result;

            //#7
            ok(compare(data.length, 1));

        console.log("@6");

            res = remove_individual(ticket_user1.id, new_test_doc1['@']);
            //wait_module(m_scripts, res.op_id);

            //#8
            test_fail_read(ticket_user1, new_test_doc1['@'], new_test_doc1);

            //var data = query(ticket_user1.id, "'rdfs:comment' == 'comment*' && 'v-s:deleted' == true && 'v-s:test_group' == '" + test_group_uid + "'" , undefined, undefined, true).result;

            //#9
            //ok(compare(data.length, 1));

        console.log("@7");

            var data = query(ticket_user1.id, "'rdfs:comment' == 'comm1*' && 'v-s:test_group' === '" + test_group_uid + "'" , undefined, undefined, true).result;

            //#10
            ok(compare(data.length, 1));

            //var data = query(ticket_user1.id, "'rdfs:comment' == 'comm1* && 'v-s:system' === true && 'v-s:test_group' === '" + test_group_uid + "'" , undefined, undefined, true).result;

            //#
            //ok(compare(data.length, 1));

            //var data = query(ticket_user1.id, "(('rdfs:comment' == 'comment*' && 'v-s:deleted' == true) || ('rdfs:comment' == 'comm1*')) && 'v-s:test_group' == '" + test_group_uid + "'" , undefined, undefined, true).result;

            //#11
            //ok(compare(data.length, 1));

            //var data = query(ticket_user1.id, "(('rdfs:comment' == 'comm1*') || ('rdfs:comment' == 'comment*' && 'v-s:deleted' == true)) && 'v-s:test_group' == '" + test_group_uid + "'" , undefined, undefined, true).result;

            //#
            //ok(compare(data.length, 2));//0


            //var data = query(ticket_user1.id, "(('rdfs:comment' == 'comm1*' && 'v-s:basic' === true) || ('rdfs:comment' == 'comment*' && 'v-s:deleted' === true)) && 'v-s:test_group' === '" + test_group_uid + "'" , undefined, undefined, true).result;

            //#
            //ok(compare(data.length, 2));//0

            //var data = query(ticket_user1.id, "(('rdfs:comment' == 'comment*' && 'v-s:deleted' === true) || ('rdfs:comment' == 'comm1*' && 'v-s:basic' === true)) && 'v-s:test_group' === '" + test_group_uid + "'" , undefined, undefined, true).result;

            //#
            //ok(compare(data.length, 2));//0
        console.log("@8");

            res = remove_individual(ticket_user1.id, new_test_doc5['@']);
            //wait_module(m_scripts, res.op_id);

            //#12
            test_fail_read(ticket_user1, new_test_doc5['@'], new_test_doc5);

            //var data = query(ticket_user1.id, "(('rdfs:comment' == 'comment*' && 'v-s:deleted' === true) || ('rdfs:comment' == 'comm1*' && 'v-s:basic' === true)) && 'v-s:test_group' === '" + test_group_uid + "'" , undefined, undefined, true).result;
            //
            //#
            //ok(compare(data.length, 2));//0

            //var data = query(ticket_user1.id, "(('rdfs:comment' == 'comment*' && 'v-s:deleted' === true) || ('rdfs:comment' == 'comm1*')) && 'v-s:test_group' === '" + test_group_uid + "'" , undefined, undefined, true).result;

            //#13
            //ok(compare(data.length, 2));

        console.log("@9");

            res = remove_individual(ticket_user1.id, new_test_doc2['@']);
            //wait_module(m_scripts, res.op_id);

            //#14
            test_fail_read(ticket_user1, new_test_doc2['@'], new_test_doc2);

            res = remove_individual(ticket_user1.id, new_test_doc3['@']);
            //wait_module(m_scripts, res.op_id);

            //#15
            test_fail_read(ticket_user1, new_test_doc3['@'], new_test_doc3);

            res = remove_individual(ticket_user1.id, new_test_doc4['@']);
            //wait_module(m_scripts, res.op_id);

            //#16
            test_fail_read(ticket_user1, new_test_doc4['@'], new_test_doc4);

            res = remove_individual(ticket_user1.id, new_test_doc6['@']);
            //wait_module(m_scripts, res.op_id);

            //#17
            test_fail_read(ticket_user1, new_test_doc6['@'], new_test_doc6);

        console.log("@10");

        }
    );

    test(
        "#004 Individual store user1 and no read user2, +lang",
        function()
        {
            var ticket_user1 = get_user1_ticket();

            //#1
            ok(ticket_user1.id.length > 0);

            var ticket_user2 = get_user2_ticket();

            //#2
            ok(ticket_user2.id.length > 0);

            var new_test_doc1_uri = "test3:" + guid();
            var new_test_doc1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri('rdfs:Resource'),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:test_field': newStr('test data', 'EN')
            };

            var res = put_individual(ticket_user1.id, new_test_doc1);
            wait_module(m_scripts, res.op_id);
            wait_module(m_acl, res.op_id);

            //#3
            test_success_read(ticket_user1, new_test_doc1['@'], new_test_doc1);

            //#4
            test_fail_read(ticket_user2, new_test_doc1['@'], new_test_doc1);

            res = remove_individual (ticket_user1.id, new_test_doc1['@']);
            //wait_module(m_scripts, res.op_id);
            //wait_module(m_acl, res.op_id);

            //#5
            test_fail_read(ticket_user1, new_test_doc1['@'], new_test_doc1);
        });

    test(
        "#005 Individual store user1 and add right, user2 successfully read it, next user1 add denied right and no user2 fail read it",
        function()
        {
            var ticket_user1 = get_user1_ticket();

            //#1
            ok(ticket_user1.id.length > 0);

            var ticket_user2 = get_user2_ticket();

            //#2
            ok(ticket_user2.id.length > 0);

            var new_test_doc1_uri = "test5:" + guid();
            var new_test_doc1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri('rdfs:Resource'),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:test_field': newStr('test data', 'NONE')
            };

            var res = put_individual(ticket_user1.id, new_test_doc1);
            wait_module(m_scripts, res.op_id);
            wait_module(m_acl, res.op_id);

            //#3
            test_fail_read(ticket_user2, new_test_doc1_uri, new_test_doc1);

            //#4
            test_success_read(ticket_user1, new_test_doc1_uri, new_test_doc1);
            var read_individual;

            var res = addRight(ticket_user1.id, [can_read], ticket_user2.user_uri, new_test_doc1_uri);
            var new_permission = res[0];
            wait_module(m_acl, res[1].op_id);

            //#5
            test_success_read(ticket_user2, new_test_doc1_uri, new_test_doc1, true);

            new_permission["@"] = "_";
            delete new_permission["v-s:permissionObject"];
            delete new_permission["v-s:permissionSubject"];

            var right1 = get_rights(ticket_user1.id, new_test_doc1_uri);
            var right2 = get_rights(ticket_user2.id, new_test_doc1_uri);

            //#6
            ok(compare(new_permission, right2));

            new_permission['v-s:canUpdate'] = newBool(true);
            new_permission['v-s:canDelete'] = newBool(true);
            new_permission['v-s:canCreate'] = newBool(true);

            //#7
            ok(compare(new_permission, right1));

            //#8
            test_success_read(ticket_user2, new_test_doc1_uri, new_test_doc1);

            res = addRight(ticket_user1.id, [cant_read], ticket_user2.user_uri, new_test_doc1_uri);
            res = addRight(ticket_user1.id, [can_read], ticket_user2.user_uri, new_test_doc1_uri);
            wait_module(m_acl, res[1].op_id);

            //#9
            test_fail_read(ticket_user2, new_test_doc1_uri, new_test_doc1);

            //#10
            try
            {
                // test UPDATE rights
                new_test_doc1['v-s:updateCounter'] = newInteger (0);
                res = put_individual(ticket_user2.id, new_test_doc1);
                ok (false);
            }
            catch (e)
            {
                ok (true);
            }

            res = remove_individual (ticket_user1.id, new_test_doc1['@']);
            //wait_module(m_scripts, res.op_id);

            //#11
            test_fail_read(ticket_user1, new_test_doc1['@'], new_test_doc1);
        });

    test(
        "#006 Individual store user1 and read admin",
        function()
        {

            var ticket_user1 = get_user1_ticket();

            //#1
            ok(ticket_user1.id != "");

            var a_ticket = get_admin_ticket();

            var new_test_doc1_uri = "test6:" + guid();
            var new_test_doc1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri('rdfs:Resource'),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1'),
                'v-s:test_field': newStr('test data', 'NONE')
            };

            var res = put_individual(ticket_user1.id, new_test_doc1);
            wait_module(m_scripts, res.op_id);
            wait_module(m_acl, res.op_id);

            var read_individual = get_individual(ticket_user1.id, new_test_doc1_uri);

            //#2
            ok(compare(new_test_doc1, read_individual));

            read_individual = get_individual(a_ticket.id, new_test_doc1_uri);

            //#3
            ok(compare(new_test_doc1, read_individual));

            res = remove_individual (ticket_user1.id, new_test_doc1['@']);
            //wait_module(m_scripts, res.op_id);

            //#4
            test_fail_read(ticket_user1, new_test_doc1['@'], new_test_doc1);

        });

    test("#007 Individual store and read, test datatype", function()
    {
        var ticket = get_user1_ticket();

        var new_test_doc1 = create_test_document1(ticket);

        var read_individual = get_individual(ticket.id, new_test_doc1['@']);

        //#1
        ok(compare(new_test_doc1, read_individual));

        var res = remove_individual (ticket.id, new_test_doc1['@']);
        //wait_module(m_scripts, res.op_id);

        //#2
        test_fail_read(ticket, new_test_doc1['@'], new_test_doc1);
    });

    test("#008 test [v-s:PermissionStatement]: user1 store file, user2 not read file, add right for user2, add cant read right for user2",
        function()
        {
            var ticket1 = get_user1_ticket();
            var ticket2 = get_user2_ticket();

            var res;
            var new_test_doc1 = create_test_document1(ticket1);

            //#1
            res = test_success_read(ticket1, new_test_doc1['@'], new_test_doc1);

            //#2
            res = test_fail_read(ticket2, new_test_doc1['@'], new_test_doc1);

            res = addRight(ticket1.id, [can_read], ticket2.user_uri, new_test_doc1['@']);
            var op_id = res[1].op_id;
            wait_module(m_acl, res[1].op_id);

            //#3
            res = test_success_read(ticket2, new_test_doc1['@'], new_test_doc1, true);

            res = addRight(ticket1.id, [cant_read], ticket2.user_uri, new_test_doc1['@']);
            var op_id = res[1].op_id;
            wait_module(m_acl, res[1].op_id);

            //#4
            res = test_fail_read(ticket2, new_test_doc1['@'], new_test_doc1, true);

            res = remove_individual (ticket1.id, new_test_doc1['@']);
            //wait_module(m_scripts, res.op_id);

            //#5
            test_fail_read(ticket1, new_test_doc1['@'], new_test_doc1);
        });

    test(
        "#009 Individual of [v-s:NoPermissionStatement] store 3 and read 3",
        function()
        {
            var ticket = get_user1_ticket();

            var permissionSubject = "test9:" + guid();
            var permissionObject = "test9:" + guid();

            var new_test_doc1_uri = "test9:" + guid();
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
            wait_module(m_subject, res.op_id);
            wait_module(m_acl, res.op_id);

            var read_individual = get_individual(ticket.id, new_test_doc1_uri);

            //#1
            ok(compare(new_test_doc1, read_individual));

            res = remove_individual(ticket.id, new_test_doc1['@']);
            //wait_module(m_scripts, res.op_id);

            //#2
            test_fail_read(ticket, new_test_doc1['@'], new_test_doc1);

            var new_test_doc2 = new_test_doc1;
            var new_test_doc2_uri = "test9:" + guid();
            new_test_doc2['@'] = new_test_doc2_uri;
            new_test_doc2['v-s:canRead'] = newBool(false);
            var res = put_individual(ticket.id, new_test_doc2);
            wait_module(m_subject, res.op_id);
            wait_module(m_acl, res.op_id);

            read_individual = get_individual(ticket.id, new_test_doc2_uri);

            //#3
            ok(compare(new_test_doc2, read_individual));


            res = remove_individual(ticket.id, new_test_doc2['@']);
            //wait_module(m_scripts, res.op_id);

            //#4
            test_fail_read(ticket, new_test_doc2['@'], new_test_doc2);

            var new_test_doc3 = new_test_doc2;
            var new_test_doc3_uri = "test9:" + guid();
            new_test_doc3['@'] = new_test_doc3_uri;
            new_test_doc3['v-s:canRead'] = newBool(true);
            var res = put_individual(ticket.id, new_test_doc3);
            wait_module(m_subject, res.op_id);
            wait_module(m_acl, res.op_id);

            read_individual = get_individual(ticket.id, new_test_doc3_uri);

            //#5
            ok((read_individual['@'] == new_test_doc3_uri) == true);

            res = remove_individual(ticket.id, new_test_doc3['@']);
            //wait_module(m_scripts, res.op_id);

            //#6
            test_fail_read(ticket, new_test_doc3['@'], new_test_doc3);
        });

    test("#010 Individual of [v-s:Membership]",
        function()
        {
            var ticket1 = get_user1_ticket();
            var ticket2 = get_user2_ticket();

            var res;
            var new_test_doc1 = create_test_document1(ticket1);

            //#1
            res = test_success_read(ticket1, new_test_doc1['@'], new_test_doc1);

            //#2
            res = test_fail_read(ticket2, new_test_doc1['@'], new_test_doc1);

            var doc_group = 'g:doc_group_' + guid();
            var user_group = 'g:user_group_' + guid();

            res = addToGroup(ticket1, doc_group, new_test_doc1['@']);
            res = addToGroup(ticket1, user_group, ticket2.user_uri);

            var membersip1 = res[0];

            res = addRight(ticket1.id, [can_read], user_group, doc_group);
            var op_id = res[1].op_id;
            wait_module(m_acl, res[1].op_id);

            //#3
            res = test_success_read(ticket2, new_test_doc1['@'], new_test_doc1, true);

            res = removeFromGroup(ticket1, user_group, ticket2.user_uri);
            wait_module(m_acl, res[1].op_id);

            //#4
            test_fail_read(ticket2, new_test_doc1['@'], new_test_doc1, true);

            res = remove_individual (ticket1.id, new_test_doc1['@']);
            //wait_module(m_scripts, res.op_id);

            //#5
            test_fail_read(ticket1, new_test_doc1['@'], new_test_doc1);
        });

    test("#011 Individual of [v-s:NoMembership] store 3 and read 3 (this no membership)",
        function()
        {
            var ticket = get_user1_ticket();

            var memberOf = "test11:" + guid();
            var resources = "test11:" + guid();

            var new_test_doc1_uri = "test11:" + guid();
            var new_test_doc1 = {
                '@': new_test_doc1_uri,
                'rdf:type': newUri('v-s:ThisNoMembership'),
                'v-s:memberOf': newUri(memberOf),
                'v-s:resource': newUri(resources),
                'v-s:author': newUri('td:ValeriyBushenev-Programmer1')
            };

            var res = put_individual(ticket.id, new_test_doc1);
            wait_module(m_acl, res.op_id);
            wait_module(m_subject, res.op_id);
            wait_module(m_scripts, res.op_id);

            var read_individual = get_individual(ticket.id, new_test_doc1_uri);

            //#1
            ok(compare(new_test_doc1, read_individual));

            res = remove_individual(ticket.id, new_test_doc1['@']);
            //wait_module(m_scripts, res.op_id);

            //#2
            test_fail_read(ticket, new_test_doc1['@'], new_test_doc1);

            var new_test_doc2 = new_test_doc1;
            var new_test_doc2_uri = "test11:" + guid();
            new_test_doc2['@'] = new_test_doc2_uri;
            new_test_doc2['v-s:memberOf'] = newUri("test11:" + guid());
            var res = put_individual(ticket.id, new_test_doc2);
            wait_module(m_acl, res.op_id);
            wait_module(m_subject, res.op_id);
            wait_module(m_scripts, res.op_id);

            read_individual = get_individual(ticket.id, new_test_doc2_uri);

            //#3
            ok(compare(new_test_doc2, read_individual));

            res = remove_individual(ticket.id, new_test_doc2['@']);
            //wait_module(m_scripts, res.op_id);

            //#4
            test_fail_read(ticket, new_test_doc2['@'], new_test_doc2);

            var new_test_doc3 = new_test_doc2;
            var new_test_doc3_uri = "test11:" + guid();
            new_test_doc3['@'] = new_test_doc3_uri;
            new_test_doc3['v-s:memberOf'] = newUri(memberOf);
            var res = put_individual(ticket.id, new_test_doc3);
            wait_module(m_subject, res.op_id);
            wait_module(m_acl, res.op_id);
            wait_module(m_scripts, res.op_id);

            read_individual = get_individual(ticket.id, new_test_doc3_uri);

            //#5
            ok((read_individual['@'] == new_test_doc3_uri) == true);

            res = remove_individual(ticket.id, new_test_doc3['@']);
            //wait_module(m_scripts, res.op_id);

            //#6
            test_fail_read(ticket, new_test_doc3['@'], new_test_doc3);
        });

}
