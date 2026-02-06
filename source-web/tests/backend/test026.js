export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test(`#026 Check different group subtrees (1)`, async () => {
    // Stage 1: Login as admin and create document + setup groups
    await Helpers.get_admin_ticket();
    // Get user1 URI
    const ticket1_info = await Backend.authenticate('bushenevvt', 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
    const user1_uri = ticket1_info.user_uri;
    await Backend.logout();
    const ticket_admin = await Helpers.get_admin_ticket();

    const new_test_doc1 = await Helpers.create_test_document1(ticket_admin);

    const group_A = 'g:group_A' + Util.guid();
    const group_B = 'g:group_B' + Util.guid();
    const group_C = 'g:group_C' + Util.guid();

    await Helpers.addToGroup(group_A, new_test_doc1['@'], ['v-s:canRead']);
    await Helpers.addToGroup(group_B, group_A, ['v-s:canRead']);
    await Helpers.addToGroup(group_C, new_test_doc1['@']);
    await Helpers.addToGroup(group_B, group_C);

    const res = await Helpers.addRight(user1_uri, group_B, ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Stage 2: Login as user1 and check rights
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.check_rights_success(new_test_doc1['@'], ['v-s:canRead']);
    await Helpers.check_rights_success(new_test_doc1['@'], ['v-s:canUpdate']);
    await Helpers.check_rights_success(new_test_doc1['@'], ['v-s:canDelete']);

    // Stage 3: Login as admin for cleanup
    await Backend.logout();
    await Helpers.get_admin_ticket();

    await Backend.remove_individual(new_test_doc1['@']);
    await Helpers.test_fail_read(new_test_doc1);
  });
};
