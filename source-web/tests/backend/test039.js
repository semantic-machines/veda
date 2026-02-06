export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test(
`#039 User1 stores individual, user1 should read individual.
       User2 should fail to remove individual.`,
  async () => {
    // Stage 1: Login as user1 and create document
    await Helpers.get_user1_ticket();

    const new_test_doc1_uri = 'test6:' + Util.guid();
    const new_test_doc1 = {
      '@': new_test_doc1_uri,
      'rdf:type': Util.newUri('rdfs:Resource'),
      'v-s:author': Util.newUri('td:ValeriyBushenev-Programmer1'),
      'v-s:created': Util.newDate(new Date()),
      'v-s:test_field': Util.newStr('test data', 'NONE'),
    };

    const res = await Backend.put_individual(new_test_doc1);
    assert(await Backend.wait_module(Constants.m_scripts, res.op_id));
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    await Helpers.test_success_read(new_test_doc1);

    // Stage 2: Login as user2 and try to access
    await Backend.logout();
    await Helpers.get_user2_ticket();

    // User2 should fail to read (no rights)
    await Helpers.test_fail_read(new_test_doc1);

    // User2 should fail to remove
    await assert.rejects(Backend.remove_individual(new_test_doc1_uri));

    // Stage 3: Login back as user1 and verify document still exists
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.test_success_read(new_test_doc1);
  });
};
