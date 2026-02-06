export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test('#045 get_rights', async () => {
    // Get user URIs
    const ticket_user1 = await Helpers.get_user1_ticket();
    const user1_uri = ticket_user1.user_uri;
    const ticket_user2 = await Helpers.get_user2_ticket();
    const user2_uri = ticket_user2.user_uri;

    // Switch back to user1 to create document
    await Backend.logout();
    await Helpers.get_user1_ticket();

    const new_test_doc1_uri = 'test45:' + Util.guid();
    const new_test_doc1 = {
      '@': new_test_doc1_uri,
      'rdf:type': Util.newUri('rdfs:Resource'),
      'v-s:author': Util.newUri('td:ValeriyBushenev-Programmer1'),
      'v-s:created': Util.newDate(new Date()),
    };

    const res = await Backend.put_individual(new_test_doc1);
    assert(await Backend.wait_module(Constants.m_scripts, res.op_id));
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    const rights1 = await Backend.get_rights(new_test_doc1_uri);
    assert('v-s:canCreate' in rights1);
    assert('v-s:canRead' in rights1);
    assert('v-s:canUpdate' in rights1);
    assert('v-s:canDelete' in rights1);

    // Note: Cross-user rights check with user_uri parameter need server-side support
    const rights2 = await Backend.get_rights(new_test_doc1_uri, user1_uri);
    assert('v-s:canCreate' in rights2);
    assert('v-s:canRead' in rights2);
    assert('v-s:canUpdate' in rights2);
    assert('v-s:canDelete' in rights2);

    const rights3 = await Backend.get_rights(new_test_doc1_uri, user2_uri);
    assert(!('v-s:canCreate' in rights3));
    assert(!('v-s:canRead' in rights3));
    assert(!('v-s:canUpdate' in rights3));
    assert(!('v-s:canDelete' in rights3));

    await Backend.remove_individual(new_test_doc1_uri);
  });
};
