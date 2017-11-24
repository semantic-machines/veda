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

}
