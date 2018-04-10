// QUnit veda server docflow stress test

QUnit.module('Stress test docflow');

function stress_test_docflow1()
{
    var ticket = get_admin_ticket();

    for (var idx = 0; idx < 1000; idx++)
    {
        var new_doc_id = 'td:ComplexRouteStartForm' + idx;
        var new_test_doc1 = {
            '@': new_doc_id,
            'rdf:type': [
                {
                    data: 'mnd-wf:ComplexRouteStartForm',
                    type: _Uri
                                },
                {
                    data: 'v-wf:StartForm',
                    type: _Uri
                                }],
            'v-wf:forNet': [
            {
                data: 'mnd-wf:net_complex_route',
                type: _Uri
                                }],
            'v-wf:processedDocument': [
            {
                data: new_doc_id,
                type: _Uri
                                }],
            'v-wf:processInitiator': [
            {
                data: 'td:RomanKarpov-Analyst1',
                type: _Uri
                                }],
            'v-wf:useTransformation': [
            {
                data: 'mnd-wf:transformation1',
                type: _Uri
                                }],
            'mnd-wf:ComplexRouteStartForm_coordinator1': [
            {
                data: 'td:RomanKarpov-Analyst1',
                type: _Uri
                                },
            {
                data: 'td:ValeriyBushenev-Programmer1',
                type: _Uri
                                }],
            'mnd-wf:ComplexRouteStartForm_coordinator2': [
            {
                data: 'td:ValeriyBushenev-Programmer1',
                type: _Uri
                                }],
            'mnd-wf:ComplexRouteStartForm_coordinator3': [
            {
                data: 'td:AndreyBychin-ExecutiveDirector',
                type: _Uri
                                }],
            'mnd-wf:ComplexRouteStartForm_reviewer': [
            {
                data: 'td:ValeriyBushenev-Programmer1',
                type: _Uri
                                }],
            'mnd-wf:ComplexRouteStartForm_signer': [
            {
                data: 'td:AndreyBychin-ExecutiveDirector',
                type: _Uri
                                }]
        };

        put_individual(ticket.id, new_test_doc1, false);
    }

}
