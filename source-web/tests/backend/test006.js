export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test(
`#006 User1 stores individual and adds right [R] for user2, user2 should successfully read individual.
       User1 adds forbidding right [!R], user2 should fail to read individual`,
  async () => {
    // Stage 1: Login as user1 and create document
    const ticket_user1 = await Helpers.get_user1_ticket();
    const ticket_user2_info = await Backend.authenticate('BychinAt', 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
    const user2_uri = ticket_user2_info.user_uri;
    // Re-login as user1
    await Backend.logout();
    await Helpers.get_user1_ticket();

    const new_test_doc1_uri = 'test5:' + Util.guid();
    const new_test_doc1 = {
      '@': new_test_doc1_uri,
      'rdf:type': Util.newUri('rdfs:Resource'),
      'v-s:author': Util.newUri('td:ValeriyBushenev-Programmer1'),
      'v-s:created': Util.newDate(new Date()),
      'v-s:test_field': Util.newStr('test data', 'NONE'),
    };

    let res;

    res = await Backend.put_individual(new_test_doc1);
    assert(await Backend.wait_module(Constants.m_scripts, res.op_id));
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    let server_test_doc1;

    server_test_doc1 = await Backend.get_individual(new_test_doc1_uri);
    assert(Helpers.compare(new_test_doc1, server_test_doc1));

    // Stage 2: Logout and login as user2 - should fail to read (no rights)
    await Backend.logout();
    await Helpers.get_user2_ticket();

    await assert.rejects(Backend.get_individual(new_test_doc1_uri));

    // Stage 3: Login as user1 and add read rights for user2
    await Backend.logout();
    await Helpers.get_user1_ticket();

    res = await Helpers.addRight(user2_uri, new_test_doc1_uri, ['v-s:canRead']);
    const new_permission = res[0];
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Stage 4: Login as user2 - should read successfully
    await Backend.logout();
    await Helpers.get_user2_ticket();

    server_test_doc1 = await Backend.get_individual(new_test_doc1_uri);
    assert(Helpers.compare(new_test_doc1, server_test_doc1));

    // Check rights for user2
    const right2 = await Backend.get_rights(new_test_doc1_uri);
    assert(right2['v-s:canRead']);

    // Stage 5: Login as user1 and add forbidding right
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.addRight(user2_uri, new_test_doc1_uri, [], ['v-s:canRead']);
    res = await Helpers.addRight(user2_uri, new_test_doc1_uri, ['v-s:canRead']);
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Stage 6: Login as user2 - should fail to read (forbidden)
    await Backend.logout();
    await Helpers.get_user2_ticket();

    await assert.rejects(Backend.get_individual(new_test_doc1_uri));

    // Stage 7: Login as user1 for cleanup
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Backend.remove_individual(new_test_doc1_uri);
    await assert.rejects(Backend.get_individual(new_test_doc1_uri));
  });
};
