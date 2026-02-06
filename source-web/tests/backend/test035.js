export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test(`#035 Check restrictions in subject groups`, async () => {
    // Stage 1: Login as admin and get user URIs
    await Helpers.get_admin_ticket();
    const ticket1_info = await Backend.authenticate('bushenevvt', 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
    const user1_uri = ticket1_info.user_uri;
    const ticket2_info = await Backend.authenticate('BychinAt', 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
    const user2_uri = ticket2_info.user_uri;
    await Backend.logout();
    const ticket_admin = await Helpers.get_admin_ticket();

    const doc1 = (await Helpers.create_test_document1(ticket_admin))['@'];

    const group_Doc = 'g:group_A' + Util.guid();
    const group_User = 'g:group_B' + Util.guid();

    await Helpers.addToGroup(group_Doc, doc1);
    await Helpers.addToGroup(group_User, user1_uri, ['v-s:canRead']);
    await Helpers.addToGroup(group_User, user2_uri);

    const res = await Helpers.addRight(group_User, group_Doc, ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Stage 2: Login as user1 and check rights (should only have read due to restriction)
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.check_rights_success(doc1, ['v-s:canRead']);
    await Helpers.check_rights_fail(doc1, ['v-s:canUpdate']);
    await Helpers.check_rights_fail(doc1, ['v-s:canDelete']);

    // Stage 3: Login as user2 and check rights (should have full rights)
    await Backend.logout();
    await Helpers.get_user2_ticket();

    await Helpers.check_rights_success(doc1, ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);
  });
};
