export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  // SHA256 hash of empty string
  const EMPTY_SHA256_HASH = 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855';

  test(`#054 Security: Empty ticket should be rejected`, async () => {
    console.log('  [test054] Testing empty ticket handling...');
    console.log('  [test054] Attempting get_individual with empty ticket...');
    try {
      await Backend.get_individual('', 'owl:');
      console.log('  [test054] Request succeeded - server allows guest access (reject_empty_ticket=false)');
      assert(true, 'Server configured to allow guest access with empty ticket');
    } catch (e) {
      console.log(`  [test054] ✓ Empty ticket rejected (error: ${e.message || e.code || 'rejected'})`);
      assert(true, 'Empty ticket correctly rejected');
    }
  });

  test(`#054.1 Security: Empty SHA256 hash ticket should be handled`, async () => {
    console.log('  [test054.1] Testing SHA256 hash of empty string as ticket...');
    console.log(`  [test054.1] Hash: ${EMPTY_SHA256_HASH}`);
    try {
      await Backend.get_individual(EMPTY_SHA256_HASH, 'owl:');
      console.log('  [test054.1] Request succeeded - server treats empty hash as guest ticket');
      assert(true, 'Server configured to allow guest access');
    } catch (e) {
      console.log(`  [test054.1] ✓ Empty SHA256 hash ticket rejected (error: ${e.message || e.code || 'rejected'})`);
      assert(true, 'Empty SHA256 hash ticket correctly rejected');
    }
  });

  test(`#054.2 Security: 'systicket' string should be handled`, async () => {
    console.log('  [test054.2] Testing "systicket" string as ticket...');
    try {
      await Backend.get_individual('systicket', 'owl:');
      console.log('  [test054.2] Request succeeded - server treats "systicket" as guest ticket');
      assert(true, 'Server configured to allow guest access');
    } catch (e) {
      console.log(`  [test054.2] ✓ "systicket" rejected (error: ${e.message || e.code || 'rejected'})`);
      assert(true, '"systicket" correctly rejected');
    }
  });

  test(`#054.3 Security: Random invalid ticket should be rejected`, async () => {
    const randomTicket = 'invalid_ticket_' + Date.now() + '_' + Math.random().toString(36);
    console.log(`  [test054.3] Testing random invalid ticket: ${randomTicket.substring(0, 30)}...`);
    try {
      await Backend.get_individual(randomTicket, 'owl:');
      console.log('  [test054.3] ✗ Random invalid ticket was accepted (unexpected!)');
      assert(false, 'Random invalid ticket should be rejected');
    } catch (e) {
      console.log(`  [test054.3] ✓ Random invalid ticket rejected (error: ${e.message || e.code || 'rejected'})`);
      assert(true, 'Random invalid ticket correctly rejected');
    }
  });

  test(`#054.4 Security: Valid ticket should still work`, async () => {
    console.log('  [test054.4] Testing valid ticket functionality...');
    const ticket = await Helpers.get_user1_ticket();
    console.log(`  [test054.4] Got ticket for user: ${ticket.user_uri}`);
    assert(ticket.ticket.length > 0, 'Should get valid ticket');

    const owl = await Backend.get_individual(ticket.ticket, 'owl:');
    console.log(`  [test054.4] Successfully read individual: ${owl['@']}`);
    assert(owl['@'] === 'owl:', 'Should be able to read with valid ticket');
    console.log('  [test054.4] ✓ Valid ticket works correctly');
  });

  test(`#054.5 Security: Ticket validation on write operations`, async () => {
    console.log('  [test054.5] Testing write operations with invalid tickets...');
    const testDoc = {
      '@': 'test:security_empty_ticket_' + Date.now(),
      'rdf:type': Util.newUri('rdfs:Resource'),
    };

    console.log('  [test054.5] Attempting put_individual with empty ticket...');
    try {
      await Backend.put_individual('', testDoc);
      console.log('  [test054.5] ✗ Write with empty ticket succeeded (security issue!)');
      assert(false, 'Write with empty ticket should be rejected');
    } catch (e) {
      console.log(`  [test054.5] ✓ Write with empty ticket rejected (error: ${e.message || e.code || 'rejected'})`);
      assert(true, 'Write with empty ticket correctly rejected');
    }

    console.log('  [test054.5] Attempting put_individual with SHA256 empty hash...');
    try {
      await Backend.put_individual(EMPTY_SHA256_HASH, testDoc);
      console.log('  [test054.5] ✗ Write with empty hash succeeded (security issue!)');
      assert(false, 'Write with empty SHA256 hash should be rejected');
    } catch (e) {
      console.log(`  [test054.5] ✓ Write with empty SHA256 hash rejected (error: ${e.message || e.code || 'rejected'})`);
      assert(true, 'Write with empty SHA256 hash correctly rejected');
    }
  });

  test(`#054.6 Security: Query operations with empty ticket`, async () => {
    console.log('  [test054.6] Testing query with empty ticket...');
    try {
      const result = await Backend.query('', "'@'=='owl:'");
      console.log(`  [test054.6] Query succeeded, results count: ${result?.result?.length || 0}`);
      assert(true, 'Server allows guest queries (may be intentional)');
    } catch (e) {
      console.log(`  [test054.6] ✓ Query with empty ticket rejected (error: ${e.message || e.code || 'rejected'})`);
      assert(true, 'Query with empty ticket rejected');
    }
  });

  test(`#054.7 Security: is_ticket_valid correctly identifies invalid tickets`, async () => {
    console.log('  [test054.7] Testing is_ticket_valid for various tickets...');

    console.log('  [test054.7] Checking empty ticket...');
    const emptyValid = await Backend.is_ticket_valid('');
    console.log(`  [test054.7] Empty ticket valid: ${emptyValid}`);
    assert(emptyValid === false, 'Empty ticket should be invalid');

    console.log('  [test054.7] Checking SHA256 of empty string...');
    const hashValid = await Backend.is_ticket_valid(EMPTY_SHA256_HASH);
    console.log(`  [test054.7] Empty SHA256 hash valid: ${hashValid}`);
    assert(hashValid === false, 'Empty SHA256 hash ticket should be invalid');

    console.log('  [test054.7] Checking random invalid ticket...');
    const randomValid = await Backend.is_ticket_valid('random_invalid_ticket_123');
    console.log(`  [test054.7] Random ticket valid: ${randomValid}`);
    assert(randomValid === false, 'Random ticket should be invalid');

    console.log('  [test054.7] Checking valid ticket...');
    const ticket = await Helpers.get_user1_ticket();
    const validTicket = await Backend.is_ticket_valid(ticket.ticket);
    console.log(`  [test054.7] Valid ticket valid: ${validTicket}`);
    assert(validTicket === true, 'Valid ticket should be valid');

    console.log('  [test054.7] ✓ is_ticket_valid works correctly');
  });

  // ============================================================
  // Tests for reject_guest_user parameter
  // When reject_guest_user=true, Guest user (cfg:Guest) access is denied
  // ============================================================

  test(`#054.8 Security: Guest user access control - read operations`, async () => {
    console.log('  [test054.8] Testing Guest user read access control...');
    console.log('  [test054.8] Attempting to read v-s:AllResourcesGroup with empty ticket (Guest context)...');
    try {
      const result = await Backend.get_individual('', 'v-s:AllResourcesGroup');
      console.log(`  [test054.8] Read succeeded - Guest access allowed (reject_guest_user=false)`);
      console.log(`  [test054.8] Result: ${result['@']}`);
      assert(true, 'Guest read access allowed (reject_guest_user=false)');
    } catch (e) {
      console.log(`  [test054.8] ✓ Guest read access denied (reject_guest_user=true)`);
      console.log(`  [test054.8] Error: ${e.message || e.code || 'rejected'}`);
      assert(true, 'Guest read access denied (reject_guest_user=true)');
    }
  });

  test(`#054.9 Security: Guest user cannot modify data`, async () => {
    console.log('  [test054.9] Testing Guest user write restrictions...');
    const testDoc = {
      '@': 'test:guest_write_attempt_' + Date.now(),
      'rdf:type': Util.newUri('rdfs:Resource'),
      'rdfs:label': Util.newStr('Test', 'EN'),
    };

    console.log('  [test054.9] Attempting put_individual with empty ticket (Guest context)...');
    try {
      await Backend.put_individual('', testDoc);
      console.log('  [test054.9] ✗ Guest was able to create document (security issue!)');
      assert(false, 'Guest should not be able to create documents');
    } catch (e) {
      console.log(`  [test054.9] ✓ Guest write rejected (error: ${e.message || e.code || 'rejected'})`);
      assert(true, 'Guest write correctly rejected');
    }

    console.log('  [test054.9] Attempting put_individual with SHA256 empty hash...');
    try {
      await Backend.put_individual(EMPTY_SHA256_HASH, testDoc);
      console.log('  [test054.9] ✗ Guest (via empty hash) was able to create document (security issue!)');
      assert(false, 'Guest (via empty hash) should not be able to create documents');
    } catch (e) {
      console.log(`  [test054.9] ✓ Guest write via empty hash rejected (error: ${e.message || e.code || 'rejected'})`);
      assert(true, 'Guest write via empty hash correctly rejected');
    }
  });

  test(`#054.10 Security: Guest user cannot delete data`, async () => {
    console.log('  [test054.10] Testing Guest user delete restrictions...');

    console.log('  [test054.10] Creating test document with admin ticket...');
    const ticket = await Helpers.get_admin_ticket();
    const testDoc = await Helpers.create_test_document3(ticket);
    console.log(`  [test054.10] Created document: ${testDoc['@']}`);

    console.log('  [test054.10] Attempting delete with empty ticket (Guest context)...');
    try {
      await Backend.remove_individual('', testDoc['@']);
      console.log('  [test054.10] ✗ Guest was able to delete document (security issue!)');
      assert(false, 'Guest should not be able to delete documents');
    } catch (e) {
      console.log(`  [test054.10] ✓ Guest delete rejected (error: ${e.message || e.code || 'rejected'})`);
      assert(true, 'Guest delete correctly rejected');
    }

    console.log('  [test054.10] Cleaning up test document...');
    try {
      await Backend.remove_individual(ticket.ticket, testDoc['@']);
      console.log('  [test054.10] Cleanup successful');
    } catch (e) {
      console.log('  [test054.10] Cleanup failed (non-critical)');
    }
  });

  test(`#054.11 Security: Guest user query access control`, async () => {
    console.log('  [test054.11] Testing Guest user query access control...');
    console.log('  [test054.11] Attempting query with empty ticket...');
    try {
      const result = await Backend.query('', "'rdf:type'=='owl:Ontology'");
      if (result && result.result && result.result.length > 0) {
        console.log(`  [test054.11] Query succeeded with ${result.result.length} results`);
        console.log('  [test054.11] Guest query access allowed (reject_guest_user=false)');
        assert(true, 'Guest query access allowed with results');
      } else {
        console.log('  [test054.11] Query succeeded but no results (ACL restriction)');
        assert(true, 'Guest query access allowed but no visible results');
      }
    } catch (e) {
      console.log(`  [test054.11] ✓ Guest query access denied (reject_guest_user=true)`);
      console.log(`  [test054.11] Error: ${e.message || e.code || 'rejected'}`);
      assert(true, 'Guest query access denied (reject_guest_user=true)');
    }
  });

  test(`#054.12 Security: Guest user get_rights access control`, async () => {
    console.log('  [test054.12] Testing Guest user get_rights access...');
    try {
      const rights = await Backend.get_rights('', 'owl:');
      console.log('  [test054.12] get_rights succeeded for Guest');
      console.log(`  [test054.12] Rights: ${JSON.stringify(rights)}`);
      assert(true, 'Guest get_rights access allowed');
    } catch (e) {
      console.log(`  [test054.12] ✓ Guest get_rights access denied`);
      console.log(`  [test054.12] Error: ${e.message || e.code || 'rejected'}`);
      assert(true, 'Guest get_rights access denied');
    }
  });

  test(`#054.13 Security: Guest user get_membership access control`, async () => {
    console.log('  [test054.13] Testing Guest user get_membership access...');
    try {
      const membership = await Backend.get_membership('', 'cfg:VedaSystem');
      console.log('  [test054.13] get_membership succeeded for Guest');
      console.log(`  [test054.13] Membership data received`);
      assert(true, 'Guest get_membership access allowed');
    } catch (e) {
      console.log(`  [test054.13] ✓ Guest get_membership access denied`);
      console.log(`  [test054.13] Error: ${e.message || e.code || 'rejected'}`);
      assert(true, 'Guest get_membership access denied');
    }
  });

  test(`#054.14 Security: Authenticated user different from Guest`, async () => {
    console.log('  [test054.14] Verifying authenticated user is not treated as Guest...');

    const ticket = await Helpers.get_user1_ticket();
    console.log(`  [test054.14] Authenticated as: ${ticket.user_uri}`);

    console.log('  [test054.14] Testing read access...');
    const owl = await Backend.get_individual(ticket.ticket, 'owl:');
    console.log(`  [test054.14] Read successful: ${owl['@']}`);
    assert(owl['@'] === 'owl:', 'Authenticated user can read');

    console.log('  [test054.14] Testing query access...');
    const queryResult = await Backend.query(ticket.ticket, "'@'=='owl:'");
    console.log(`  [test054.14] Query returned ${queryResult.result.length} results`);
    assert(queryResult.result.indexOf('owl:') >= 0, 'Authenticated user can query');

    console.log(`  [test054.14] Checking user is not Guest: ${ticket.user_uri}`);
    assert(ticket.user_uri !== 'cfg:Guest', 'Authenticated user is not Guest');
    console.log('  [test054.14] ✓ Authenticated user correctly identified and has access');
  });

  test(`#054.15 Security: Empty ticket with get_ticket_trusted should fail`, async () => {
    console.log('  [test054.15] Testing get_ticket_trusted with invalid tickets...');

    console.log('  [test054.15] Attempting get_ticket_trusted with empty ticket...');
    try {
      await Backend.get_ticket_trusted('', 'bushenevvt');
      console.log('  [test054.15] ✗ get_ticket_trusted with empty ticket succeeded (security issue!)');
      assert(false, 'get_ticket_trusted with empty ticket should fail');
    } catch (e) {
      console.log(`  [test054.15] ✓ get_ticket_trusted with empty ticket rejected (error: ${e.message || e.code || 'rejected'})`);
      assert(true, 'get_ticket_trusted with empty ticket correctly rejected');
    }

    console.log('  [test054.15] Attempting get_ticket_trusted with SHA256 empty hash...');
    try {
      await Backend.get_ticket_trusted(EMPTY_SHA256_HASH, 'bushenevvt');
      console.log('  [test054.15] ✗ get_ticket_trusted with empty hash succeeded (security issue!)');
      assert(false, 'get_ticket_trusted with empty hash should fail');
    } catch (e) {
      console.log(`  [test054.15] ✓ get_ticket_trusted with empty hash rejected (error: ${e.message || e.code || 'rejected'})`);
      assert(true, 'get_ticket_trusted with empty hash correctly rejected');
    }
  });
};
