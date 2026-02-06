export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test(`#028 Check different group subtrees (3)`, async () => {
    // Stage 1: Login as admin and create documents + setup groups
    await Helpers.get_admin_ticket();
    // Get user1 URI
    const ticket1_info = await Backend.authenticate('bushenevvt', 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
    const user1_uri = ticket1_info.user_uri;
    await Backend.logout();
    const ticket_admin = await Helpers.get_admin_ticket();

    const doc1 = (await Helpers.create_test_document1(ticket_admin))['@'];
    const doc2 = (await Helpers.create_test_document1(ticket_admin))['@'];
    const group_A = 'g:group_A' + Util.guid();
    const group_B = 'g:group_B' + Util.guid();

    await Helpers.addToGroup(doc2, doc1, ['v-s:canRead']);
    await Helpers.addToGroup(group_A, doc2, ['v-s:canRead']);
    await Helpers.addToGroup(group_B, group_A, ['v-s:canRead']);
    await Helpers.addToGroup(group_B, doc2);

    const res = await Helpers.addRight(user1_uri, group_B, ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Stage 2: Login as user1 and check rights
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.check_rights_success(doc1, ['v-s:canRead']);
    await Helpers.check_rights_fail(doc1, ['v-s:canUpdate']);
    await Helpers.check_rights_fail(doc1, ['v-s:canDelete']);
  });
};
