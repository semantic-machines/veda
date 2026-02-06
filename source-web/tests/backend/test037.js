export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test(`#037 Check rights delete & restore`, async () => {
    // Stage 1: Login as admin and get user1 URI
    await Helpers.get_admin_ticket();
    const ticket1_info = await Backend.authenticate('bushenevvt', 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
    const user1 = ticket1_info.user_uri;
    await Backend.logout();
    const ticket_admin = await Helpers.get_admin_ticket();

    const doc1 = (await Helpers.create_test_document1(ticket_admin))['@'];

    let res;

    // Right1 = U
    res = await Helpers.addRight(user1, doc1, ['v-s:canUpdate']);
    const right1 = res[0];
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Right2 = R
    res = await Helpers.addRight(user1, doc1, ['v-s:canRead']);
    const right2 = res[0];
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Delete Right2 with RU first time
    right2['v-s:canUpdate'] = Util.newBool(true);
    right2['v-s:deleted'] = Util.newBool(true);

    res = await Backend.put_individual(right2);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // Stage 2: Login as user1 and check rights
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.check_rights_fail(doc1, ['v-s:canRead']);
    await Helpers.check_rights_success(doc1, ['v-s:canUpdate']);

    // Stage 3: Login as admin and delete Right2 second time
    await Backend.logout();
    await Helpers.get_admin_ticket();

    res = await Backend.put_individual(right2);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // Stage 4: Login as user1 and check rights
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.check_rights_fail(doc1, ['v-s:canRead']);
    await Helpers.check_rights_success(doc1, ['v-s:canUpdate']);

    // Stage 5: Login as admin and restore Right2 with RUD
    await Backend.logout();
    await Helpers.get_admin_ticket();

    delete right2['v-s:deleted'];
    right2['v-s:canDelete'] = Util.newBool(true);
    res = await Backend.put_individual(right2);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // Stage 6: Login as user1 and check full rights
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.check_rights_success(doc1, ['v-s:canRead']);
    await Helpers.check_rights_success(doc1, ['v-s:canUpdate']);
    await Helpers.check_rights_success(doc1, ['v-s:canDelete']);

    // Stage 7: Login as admin and remove right1
    await Backend.logout();
    await Helpers.get_admin_ticket();

    res = await Backend.remove_individual(right1['@']);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // Stage 8: Login as user1 and verify rights still exist from right2
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.check_rights_success(doc1, ['v-s:canRead']);
    await Helpers.check_rights_success(doc1, ['v-s:canUpdate']);
    await Helpers.check_rights_success(doc1, ['v-s:canDelete']);
  });
};
