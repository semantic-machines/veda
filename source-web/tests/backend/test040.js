export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test(`#040 Check Membership dropCount`, async () => {
    // Stage 1: Login as admin and create document
    await Helpers.get_admin_ticket();
    // Get user1 URI
    const ticket1_info = await Backend.authenticate('bushenevvt', 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
    const user1_uri = ticket1_info.user_uri;
    await Backend.logout();
    const ticket_admin = await Helpers.get_admin_ticket();

    const doc_group = 'g:doc_resource_group';
    const doc = await Helpers.create_test_document3(ticket_admin);

    let res;

    await Helpers.test_success_read(doc);

    // Stage 2: Login as user1 - should not have access yet
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.test_fail_read(doc);

    // Stage 3: Login as admin and setup group membership
    await Backend.logout();
    await Helpers.get_admin_ticket();

    // doc добавляем в doc_gr с RUD
    await Helpers.addToGroup(doc_group, doc['@'], ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);

    // User RUD doc_gr
    res = await Helpers.addRight(user1_uri, doc_group, ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);
    assert(await Backend.wait_module(Constants.m_acl, res[1].op_id));

    // Stage 4: Login as user1 and check rights
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.check_rights_success(doc_group, ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);
    await Helpers.check_rights_success(doc['@'], ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);

    // Stage 5: Login as admin and restrict UD via dropCount
    await Backend.logout();
    await Helpers.get_admin_ticket();

    // doc  в doc_gr с R, UD обнуляем
    const test_memb1_dropCount_uri = Util.genUri();
    const test_memb1_dropCount = {
      '@': test_memb1_dropCount_uri,
      'rdf:type': Util.newUri('v-s:Membership'),
      'v-s:dropCount': Util.newBool(true),
      'v-s:resource': Util.newUri(doc['@']),
      'v-s:memberOf': Util.newUri(doc_group),
      'v-s:canUpdate': Util.newBool(true),
      'v-s:canDeleted': Util.newBool(true),
      'v-s:deleted': Util.newBool(true),
    };
    res = await Backend.put_individual(test_memb1_dropCount);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // Stage 6: Login as user1 and check restricted rights
    await Backend.logout();
    await Helpers.get_user1_ticket();

    // У User есть RUD на doc_group
    await Helpers.check_rights_success(doc_group, ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);

    // У User есть R на doc
    await Helpers.check_rights_success(doc['@'], ['v-s:canRead']);

    // У User нет UD на doc
    await Helpers.check_rights_fail(doc['@'], ['v-s:canUpdate', 'v-s:canDelete']);

    // Stage 7: Login as admin and restore full rights
    await Backend.logout();
    await Helpers.get_admin_ticket();

    // doc  в doc_gr с RUD
    const test_memb2_dropCount_uri = Util.genUri();
    const test_memb2_dropCount = {
      '@': test_memb2_dropCount_uri,
      'rdf:type': Util.newUri('v-s:Membership'),
      'v-s:dropCount': Util.newBool(true),
      'v-s:resource': Util.newUri(doc['@']),
      'v-s:memberOf': Util.newUri(doc_group),
    };
    res = await Backend.put_individual(test_memb2_dropCount);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // Stage 8: Login as user1 and check restored rights
    await Backend.logout();
    await Helpers.get_user1_ticket();

    // У User есть RUD на doc
    await Helpers.check_rights_success(doc['@'], ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);

    // Stage 9: Login as admin and drop all group memberships
    await Backend.logout();
    await Helpers.get_admin_ticket();

    // Обнуление всех вхождений doc в doc_gr
    const test_memb3_dropCount_uri = Util.genUri();
    const test_memb3_dropCount = {
      '@': test_memb3_dropCount_uri,
      'rdf:type': Util.newUri('v-s:Membership'),
      'v-s:dropCount': Util.newBool(true),
      'v-s:resource': Util.newUri(doc['@']),
      'v-s:memberOf': Util.newUri(doc_group),
      'v-s:deleted': Util.newBool(true),
    };
    res = await Backend.put_individual(test_memb3_dropCount);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // Stage 10: Login as user1 - no rights
    await Backend.logout();
    await Helpers.get_user1_ticket();

    // У User нет RUD на doc
    await Helpers.check_rights_fail(doc['@'], ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);

    // Stage 11: Login as admin and add back to group
    await Backend.logout();
    await Helpers.get_admin_ticket();

    // doc в doc_gr с RUD
    const test_memb4_dropCount_uri = Util.genUri();
    let test_memb4_dropCount = {
      '@': test_memb4_dropCount_uri,
      'rdf:type': Util.newUri('v-s:Membership'),
      'v-s:dropCount': Util.newBool(true),
      'v-s:resource': Util.newUri(doc['@']),
      'v-s:memberOf': Util.newUri(doc_group),
    };
    res = await Backend.put_individual(test_memb4_dropCount);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // Stage 12: Login as user1 and verify rights
    await Backend.logout();
    await Helpers.get_user1_ticket();

    await Helpers.check_rights_success(doc['@'], ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);

    // Stage 13: Login as admin and add v-s:deleted to existing membership
    await Backend.logout();
    await Helpers.get_admin_ticket();

    // В предыдущий membership добавляем v-s:deleted, пытаемся забрать права. Не должно срабатывать
    test_memb4_dropCount = {
      '@': test_memb4_dropCount_uri,
      'rdf:type': Util.newUri('v-s:Membership'),
      'v-s:dropCount': Util.newBool(true),
      'v-s:resource': Util.newUri(doc['@']),
      'v-s:memberOf': Util.newUri(doc_group),
      'v-s:deleted': Util.newBool(true),
    };
    res = await Backend.put_individual(test_memb4_dropCount);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // Stage 14: Login as user1 - rights should remain
    await Backend.logout();
    await Helpers.get_user1_ticket();

    // Права остались RUD
    await Helpers.check_rights_success(doc['@'], ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);

    // Stage 15: Login as admin and create new membership to drop all
    await Backend.logout();
    await Helpers.get_admin_ticket();

    // Создаём новый membership всё обнуляем
    const test_memb5_dropCount_uri = Util.genUri();
    const test_memb5_dropCount = {
      '@': test_memb5_dropCount_uri,
      'rdf:type': Util.newUri('v-s:Membership'),
      'v-s:dropCount': Util.newBool(true),
      'v-s:resource': Util.newUri(doc['@']),
      'v-s:memberOf': Util.newUri(doc_group),
      'v-s:canRead': Util.newBool(true),
      'v-s:canUpdate': Util.newBool(true),
      'v-s:canDeleted': Util.newBool(true),
      'v-s:deleted': Util.newBool(true),
    };
    res = await Backend.put_individual(test_memb5_dropCount);
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));

    // Stage 16: Login as user1 - no rights
    await Backend.logout();
    await Helpers.get_user1_ticket();

    // Прав нет
    await Helpers.check_rights_fail(doc['@'], ['v-s:canRead', 'v-s:canUpdate', 'v-s:canDelete']);
  });
};
