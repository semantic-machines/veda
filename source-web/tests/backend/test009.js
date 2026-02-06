export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test(
`#009 User1 stores individual, user2 should fail to read individual.
       User1 adds right [R] for user2, user2 should read individual.
       User1 adds right [!R] for user2, user2 should fail to read individual.`,
  async () => {
    // Stage 1: Login as user1 and create document
    const ticket1 = await Helpers.get_user1_ticket();
    // Get user2 URI
    const ticket2_info = await Backend.authenticate('BychinAt', 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
    const user2_uri = ticket2_info.user_uri;
    await Backend.logout();
    await Helpers.get_user1_ticket();

    const new_test_doc1 = await Helpers.create_test_document1(ticket1);

    let server_test_doc1;
    server_test_doc1 = await Backend.get_individual(new_test_doc1['@']);
    assert(Helpers.compare(new_test_doc1, server_test_doc1));

    // Stage 2: Login as user2 - should fail to read (no rights)
    await Backend.logout();
    await Helpers.get_user2_ticket();

    await assert.rejects(Backend.get_individual(new_test_doc1['@']));

    // Stage 3: Login as user1 and add read rights for user2
    await Backend.logout();
    await Helpers.get_user1_ticket();

    let res;
    res = await Helpers.addRight(user2_uri, new_test_doc1['@'], ['v-s:canRead']);
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Stage 4: Login as user2 - should read successfully
    await Backend.logout();
    await Helpers.get_user2_ticket();

    server_test_doc1 = await Backend.get_individual(new_test_doc1['@']);
    assert(Helpers.compare(new_test_doc1, server_test_doc1));

    // Stage 5: Login as user1 and add forbidding right
    await Backend.logout();
    await Helpers.get_user1_ticket();

    res = await Helpers.addRight(user2_uri, new_test_doc1['@'], [], ['v-s:canRead']);
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Stage 6: Login as user2 - should fail to read (forbidden)
    await Backend.logout();
    await Helpers.get_user2_ticket();

    await assert.rejects(Backend.get_individual(new_test_doc1['@']));

    // Stage 7: Login as user1 for cleanup
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Backend.remove_individual(new_test_doc1['@']);
    await assert.rejects(Backend.get_individual(new_test_doc1['@']));
  });
};
