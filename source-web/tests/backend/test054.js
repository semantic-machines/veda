export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test(`#054 Security: Valid authentication should work`, async () => {
    console.log('  [test054] Testing valid ticket functionality...');
    const ticket = await Helpers.get_user1_ticket();
    console.log(`  [test054] Got ticket for user: ${ticket.user_uri}`);
    assert(ticket.user_uri.length > 0, 'Should get valid ticket');

    const owl = await Backend.get_individual('owl:');
    console.log(`  [test054] Successfully read individual: ${owl['@']}`);
    assert(owl['@'] === 'owl:', 'Should be able to read with valid session');
    console.log('  [test054] ✓ Valid session works correctly');
  });

  test(`#054.1 Security: is_ticket_valid correctly identifies session state`, async () => {
    console.log('  [test054.1] Testing is_ticket_valid...');

    console.log('  [test054.1] Checking current session validity...');
    const ticket = await Helpers.get_user1_ticket();
    const validTicket = await Backend.is_ticket_valid();
    console.log(`  [test054.1] Current session valid: ${validTicket}`);
    assert(validTicket === true, 'Current session should be valid');

    console.log('  [test054.1] ✓ is_ticket_valid works correctly');
  });

  test(`#054.2 Security: Authenticated user can perform operations`, async () => {
    console.log('  [test054.2] Verifying authenticated user operations...');

    const ticket = await Helpers.get_user1_ticket();
    console.log(`  [test054.2] Authenticated as: ${ticket.user_uri}`);

    console.log('  [test054.2] Testing read access...');
    const owl = await Backend.get_individual('owl:');
    console.log(`  [test054.2] Read successful: ${owl['@']}`);
    assert(owl['@'] === 'owl:', 'Authenticated user can read');

    console.log('  [test054.2] Testing query access...');
    const queryResult = await Backend.query("'@'=='owl:'");
    console.log(`  [test054.2] Query returned ${queryResult.result.length} results`);
    assert(queryResult.result.indexOf('owl:') >= 0, 'Authenticated user can query');

    console.log(`  [test054.2] Checking user is not Guest: ${ticket.user_uri}`);
    assert(ticket.user_uri !== 'cfg:Guest', 'Authenticated user is not Guest');
    console.log('  [test054.2] ✓ Authenticated user correctly identified and has access');
  });

  test(`#054.3 Security: Document lifecycle with authenticated user`, async () => {
    console.log('  [test054.3] Testing document create/read/delete...');

    const ticket = await Helpers.get_admin_ticket();
    console.log(`  [test054.3] Authenticated as admin: ${ticket.user_uri}`);

    console.log('  [test054.3] Creating test document...');
    const testDoc = await Helpers.create_test_document3(ticket);
    console.log(`  [test054.3] Created document: ${testDoc['@']}`);

    console.log('  [test054.3] Reading back document...');
    const readDoc = await Backend.get_individual(testDoc['@']);
    assert(readDoc['@'] === testDoc['@'], 'Should read created document');

    console.log('  [test054.3] Deleting document...');
    await Backend.remove_individual(testDoc['@']);
    console.log('  [test054.3] ✓ Document lifecycle works correctly');
  });
};
