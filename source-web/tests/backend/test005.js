export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test(`#005 User1 stores individual, user2 should fail to read individual`, async () => {
    // Stage 1: Login as user1 and create document
    await Helpers.get_user1_ticket();

    const new_test_doc1_uri = 'test3:' + Util.guid();
    const new_test_doc1 = {
      '@': new_test_doc1_uri,
      'rdf:type': Util.newUri('rdfs:Resource'),
      'v-s:author': Util.newUri('td:ValeriyBushenev-Programmer1'),
      'v-s:created': Util.newDate(new Date()),
      'v-s:test_field': Util.newStr('test data', 'EN'),
    };

    const res = await Backend.put_individual(new_test_doc1);
    assert(await Backend.wait_module(Constants.m_scripts, res.op_id));
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    const server_test_doc1 = await Backend.get_individual(new_test_doc1_uri);
    assert(Helpers.compare(new_test_doc1, server_test_doc1));

    // Stage 2: Logout and login as user2
    await Backend.logout();
    await Helpers.get_user2_ticket();

    // Stage 3: User2 tries to read - should fail (no rights)
    await assert.rejects(Backend.get_individual(new_test_doc1_uri));

    // Stage 4: Logout and login back as user1 for cleanup
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Backend.remove_individual(new_test_doc1_uri);
    await assert.rejects(Backend.get_individual(new_test_doc1_uri));
  });
};
