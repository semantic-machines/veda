export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test(`#036 Check rights counter`, async () => {
    // Stage 1: Login as admin and get user1 URI
    await Helpers.get_admin_ticket();
    const ticket1_info = await Backend.authenticate('bushenevvt', 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
    const user1 = ticket1_info.user_uri;
    await Backend.logout();
    const ticket_admin = await Helpers.get_admin_ticket();

    const doc1 = (await Helpers.create_test_document1(ticket_admin))['@'];

    // Right1 = R
    let res = await Helpers.addRight(user1, doc1, ['v-s:canRead']);
    const right1 = res[0];
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Right2 = RU
    res = await Helpers.addRight(user1, doc1, ['v-s:canRead', 'v-s:canUpdate']);
    const right2 = res[0];
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Right3 = RUD
    res = await Helpers.addRight(user1, doc1, ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);
    const right3 = res[0];
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Stage 2: Login as user1 and check rights
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.check_rights_success(doc1, ['v-s:canRead']);
    await Helpers.check_rights_success(doc1, ['v-s:canUpdate']);
    await Helpers.check_rights_success(doc1, ['v-s:canDelete']);

    // Stage 3: Login as admin and remove right1
    await Backend.logout();
    await Helpers.get_admin_ticket();

    res = await Backend.remove_individual(right1['@']);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // Stage 4: Login as user1 and verify rights still exist
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.check_rights_success(doc1, ['v-s:canRead']);
    await Helpers.check_rights_success(doc1, ['v-s:canUpdate']);
    await Helpers.check_rights_success(doc1, ['v-s:canDelete']);

    // Stage 5: Login as admin and remove right2
    await Backend.logout();
    await Helpers.get_admin_ticket();

    res = await Backend.remove_individual(right2['@']);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // Stage 6: Login as user1 and verify rights still exist
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.check_rights_success(doc1, ['v-s:canRead']);
    await Helpers.check_rights_success(doc1, ['v-s:canUpdate']);
    await Helpers.check_rights_success(doc1, ['v-s:canDelete']);

    // Stage 7: Login as admin and remove right3
    await Backend.logout();
    await Helpers.get_admin_ticket();

    res = await Backend.remove_individual(right3['@']);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // Stage 8: Login as user1 - no rights
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.check_rights_fail(doc1, ['v-s:canRead']);
    await Helpers.check_rights_fail(doc1, ['v-s:canUpdate']);
    await Helpers.check_rights_fail(doc1, ['v-s:canDelete']);

    // Stage 9: Login as admin for multiple permission update & remove test
    await Backend.logout();
    await Helpers.get_admin_ticket();

    const doc2 = (await Helpers.create_test_document2(ticket_admin))['@'];

    res = await Helpers.addRight(user1, doc2, ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);
    const right4 = res[0];
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    res = await Backend.put_individual(right4);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    res = await Backend.put_individual(right4);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // Stage 10: Login as user1 and verify full rights
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.check_rights_success(doc2, ['v-s:canRead']);
    await Helpers.check_rights_success(doc2, ['v-s:canUpdate']);
    await Helpers.check_rights_success(doc2, ['v-s:canDelete']);

    // Stage 11: Login as admin and remove right4
    await Backend.logout();
    await Helpers.get_admin_ticket();

    res = await Backend.remove_individual(right4['@']);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // Stage 12: Login as user1 - no rights
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.check_rights_fail(doc2, ['v-s:canRead']);
    await Helpers.check_rights_fail(doc2, ['v-s:canUpdate']);
    await Helpers.check_rights_fail(doc2, ['v-s:canDelete']);

    // Stage 13: Login as admin and test changing permission rights
    await Backend.logout();
    await Helpers.get_admin_ticket();

    res = await Helpers.addRight(user1, doc2, ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);
    const right5 = res[0];
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Stage 14: Login as user1 and verify full rights
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.check_rights_success(doc2, ['v-s:canRead']);
    await Helpers.check_rights_success(doc2, ['v-s:canUpdate']);
    await Helpers.check_rights_success(doc2, ['v-s:canDelete']);

    // Stage 15: Login as admin and reduce rights
    await Backend.logout();
    await Helpers.get_admin_ticket();

    delete right5['v-s:canUpdate'];
    delete right5['v-s:canDelete'];

    res = await Backend.put_individual(right5);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // Stage 16: Login as user1 - only read
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.check_rights_success(doc2, ['v-s:canRead']);
    await Helpers.check_rights_fail(doc2, ['v-s:canUpdate']);
    await Helpers.check_rights_fail(doc2, ['v-s:canDelete']);
  });
};
