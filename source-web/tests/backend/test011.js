export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test(
`#011 User1 stores individual, user2 should fail to read individual.
       User1 adds individual to object group, user1 adds user2 to subject group.
       User1 adds right [R] for subject group to object group, user2 should read individual.
       User1 removes user2 from subject group, user2 should fail to read individual.
       User1 removes individual, user1 should fail to read individual.`,
  async () => {
    // Stage 1: Login as user1 and create document
    const ticket1 = await Helpers.get_user1_ticket();
    // Get user2 URI
    const ticket2_info = await Backend.authenticate('BychinAt', 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
    const user2_uri = ticket2_info.user_uri;
    await Backend.logout();
    await Helpers.get_user1_ticket();

    let res;
    const new_test_doc1 = await Helpers.create_test_document1(ticket1);

    let server_test_doc1;
    server_test_doc1 = await Backend.get_individual(new_test_doc1['@']);
    assert(Helpers.compare(new_test_doc1, server_test_doc1));

    // Stage 2: Login as user2 - should fail to read (no rights)
    await Backend.logout();
    await Helpers.get_user2_ticket();

    await assert.rejects(Backend.get_individual(new_test_doc1['@']));

    // Stage 3: Login as user1 and setup groups
    await Backend.logout();
    await Helpers.get_user1_ticket();

    const doc_group = 'g:doc_g1roup_' + Util.guid();
    const user_group = 'g:user_g1roup_' + Util.guid();

    await Helpers.addToGroup(doc_group, new_test_doc1['@']);
    await Helpers.addToGroup(user_group, user2_uri);

    res = await Helpers.addRight(user_group, doc_group, ['v-s:canRead']);
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Stage 4: Login as user2 - should read successfully via group
    await Backend.logout();
    await Helpers.get_user2_ticket();

    server_test_doc1 = await Backend.get_individual(new_test_doc1['@']);
    assert(Helpers.compare(new_test_doc1, server_test_doc1));

    // Stage 5: Login as user1 and remove user2 from group
    await Backend.logout();
    await Helpers.get_user1_ticket();

    res = await Helpers.removeFromGroup(user_group, user2_uri);
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Stage 6: Login as user2 - should fail to read (removed from group)
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
