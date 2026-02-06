export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test(`#016 Nested groups`, async () => {
    // Stage 1: Login as user1 and create documents
    const ticket1 = await Helpers.get_user1_ticket();
    // Get user2 URI
    const ticket2_info = await Backend.authenticate('BychinAt', 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
    const user2_uri = ticket2_info.user_uri;
    await Backend.logout();
    await Helpers.get_user1_ticket();

    const doc1 = await Helpers.create_test_document1(ticket1);
    const doc2 = await Helpers.create_test_document1(ticket1);
    const doc_group1_uri = 'g:doc_g1roup_' + Util.guid();

    await Helpers.test_success_read(doc1);
    await Helpers.test_success_read(doc2);

    // Stage 2: Login as user2 - should fail to read
    await Backend.logout();
    await Helpers.get_user2_ticket();

    await Helpers.test_fail_read(doc1);
    await Helpers.test_fail_read(doc2);

    // Stage 3: Login as user1 and setup nested groups + rights
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.addToGroup(doc1['@'], doc2['@']);
    await Helpers.addToGroup(doc_group1_uri, doc1['@']);

    const res = await Helpers.addRight(user2_uri, doc_group1_uri, ['v-s:canRead']);
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Stage 4: Login as user2 - should read both via nested group
    await Backend.logout();
    await Helpers.get_user2_ticket();

    await Helpers.test_success_read(doc1);
    await Helpers.test_success_read(doc2);

    // Stage 5: Login as user1 for cleanup
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Backend.remove_individual(doc1['@']);
    await Helpers.test_fail_read(doc1);

    await Backend.remove_individual(doc2['@']);
    await Helpers.test_fail_read(doc2);
  });
};
