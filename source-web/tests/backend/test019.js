export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test(`#019 Nested groups with restrictions & cycles`, async () => {
    // Stage 1: Login as user1 and create documents
    const ticket1 = await Helpers.get_user1_ticket();
    // Get user2 URI
    const ticket2_info = await Backend.authenticate('BychinAt', 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
    const user2_uri = ticket2_info.user_uri;
    await Backend.logout();
    await Helpers.get_user1_ticket();

    let res;
    const doc1 = await Helpers.create_test_document1(ticket1, 'doc1_');
    const doc2 = await Helpers.create_test_document1(ticket1, 'doc2_');
    const doc3 = await Helpers.create_test_document1(ticket1, 'doc3_');
    const doc_group1_uri = 'g:doc_g1roup1_' + Util.guid();
    const doc_group2_uri = 'g:doc_g1roup2_' + Util.guid();
    const doc_group3_uri = 'g:doc_g1roup3_' + Util.guid();

    await Helpers.test_success_read(doc1);
    await Helpers.test_success_read(doc2);
    await Helpers.test_success_read(doc3);

    // Stage 2: Login as user2 - should fail to read all
    await Backend.logout();
    await Helpers.get_user2_ticket();

    await Helpers.test_fail_read(doc1);
    await Helpers.test_fail_read(doc2);
    await Helpers.test_fail_read(doc3);

    // Stage 3: Login as user1 and setup cyclic group structure
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.addToGroup(doc2['@'], doc3['@'], ['v-s:canRead']);
    await Helpers.addToGroup(doc1['@'], doc2['@']);
    await Helpers.addToGroup(doc3['@'], doc1['@'], ['v-s:canRead']);

    await Helpers.addToGroup(doc_group1_uri, doc1['@']);
    await Helpers.addToGroup(doc_group1_uri, doc2['@']);
    await Helpers.addToGroup(doc_group1_uri, doc3['@']);

    await Helpers.addToGroup(doc_group2_uri, doc_group1_uri);
    await Helpers.addToGroup(doc_group3_uri, doc_group2_uri);

    res = await Helpers.addRight(user2_uri, doc_group1_uri, ['v-s:canRead']);
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    res = await Helpers.addRight(user2_uri, doc_group2_uri, ['v-s:canUpdate']);
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    res = await Helpers.addRight(user2_uri, doc_group3_uri, ['v-s:canDelete']);
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Stage 4: Login as user2 and check rights
    await Backend.logout();
    await Helpers.get_user2_ticket();

    await Helpers.check_rights_success(doc1['@'], ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);
    await Helpers.check_rights_success(doc3['@'], ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);

    // Stage 5: Login as user1 for cleanup
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Backend.remove_individual(doc1['@']);
    await Helpers.test_fail_read(doc1);

    await Backend.remove_individual(doc2['@']);
    await Helpers.test_fail_read(doc2);

    await Backend.remove_individual(doc3['@']);
    await Helpers.test_fail_read(doc3);
  });
};
