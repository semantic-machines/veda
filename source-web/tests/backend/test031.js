export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test(`#031 Check rights filter`, async () => {
    // Get user URIs first, then work under specific users
    const ticket_user1 = await Helpers.get_user1_ticket();
    const user1_uri = ticket_user1.user_uri;

    const ticket_user2 = await Helpers.get_user2_ticket();
    const user2_uri = ticket_user2.user_uri;

    // Switch to user1 to create document
    await Backend.logout();
    await Helpers.get_user1_ticket();

    const new_test_doc1_uri = 'test31:' + Util.guid();
    const new_test_doc1 = {
      '@': new_test_doc1_uri,
      'rdf:type': Util.newUri('rdfs:Resource'),
      'v-s:author': Util.newUri('td:ValeriyBushenev-Programmer1'),
      'v-s:created': Util.newDate(new Date()),
      'v-s:test_field': Util.newStr('test data', 'EN'),
    };

    let res;

    // User1 creates document
    res = await Backend.put_individual(new_test_doc1);
    assert(await Backend.wait_module(Constants.m_scripts, res.op_id));
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // User1 can read, user2 cannot
    await Helpers.test_success_read(new_test_doc1);

    await Backend.logout();
    await Helpers.get_user2_ticket();
    await Helpers.test_fail_read(new_test_doc1);

    // User1 (as document owner) adds rights for user2
    await Backend.logout();
    await Helpers.get_user1_ticket();

    res = await Helpers.addRight(user2_uri, new_test_doc1_uri, ['v-s:canRead', 'v-s:canUpdate']);
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Both can update now
    await Helpers.test_success_update(new_test_doc1);

    await Backend.logout();
    await Helpers.get_user2_ticket();
    await Helpers.test_success_update(new_test_doc1);

    // Switch to user1 to create permission filter
    await Backend.logout();
    await Helpers.get_user1_ticket();

    const new_permission_filter_uri = 'test31-pf:' + Util.guid();
    const new_permission_filter = {
      '@': new_permission_filter_uri,
      'rdf:type': Util.newUri('v-s:PermissionFilter'),
      'v-s:created': Util.newDate(new Date()),
      'v-s:permissionObject': Util.newUri(new_test_doc1_uri),
      'v-s:resource': Util.newUri(new_permission_filter_uri + 'xxx'),
      'v-s:canRead': Util.newBool(true),
    };
    res = await Backend.put_individual(new_permission_filter);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // After filter: both can only read, not update
    await Helpers.test_fail_update(new_test_doc1);
    await Helpers.test_success_read(new_test_doc1);

    await Backend.logout();
    await Helpers.get_user2_ticket();
    await Helpers.test_fail_update(new_test_doc1);
    await Helpers.test_success_read(new_test_doc1);

    // Switch to admin to add filtered permission for user2
    await Backend.logout();
    await Helpers.get_admin_ticket();

    let res1 = await Helpers.addRight(user2_uri, new_test_doc1_uri, ['v-s:canUpdate'], [], new_permission_filter_uri + 'xxx');
    const new_permission1 = res1[0];
    assert(await Backend.wait_module(Constants.m_acl, res1[1].op_id));

    // User1 still cannot update, but user2 CAN (has filtered permission)
    await Backend.logout();
    await Helpers.get_user1_ticket();
    await Helpers.test_fail_update(new_test_doc1);

    await Backend.logout();
    await Helpers.get_user2_ticket();
    await Helpers.test_success_update(new_test_doc1);

    // Switch to admin to disable permission with filter
    await Backend.logout();
    await Helpers.get_admin_ticket();

    new_permission1['v-s:deleted'] = Util.newBool(true);
    res1 = await Backend.put_individual(new_permission1);
    assert(await Backend.wait_module(Constants.m_acl, res1.op_id));

    // User2 cannot update anymore
    await Backend.logout();
    await Helpers.get_user2_ticket();
    await Helpers.test_fail_update(new_test_doc1);

    // Switch to admin to disable filter
    await Backend.logout();
    await Helpers.get_admin_ticket();

    new_permission_filter['v-s:deleted'] = Util.newBool(true);
    const res2 = await Backend.put_individual(new_permission_filter);
    assert(await Backend.wait_module(Constants.m_acl, res2.op_id));

    // User2 can update again (original rights restored)
    await Backend.logout();
    await Helpers.get_user2_ticket();
    await Helpers.test_success_update(new_test_doc1);
  });
};
