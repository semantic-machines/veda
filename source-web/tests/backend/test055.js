// Test that all endpoints work with both cookie and query parameter authentication
import nodeFetch from 'node-fetch';

export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test(`#055 All endpoints support cookie and query param auth`, async () => {
    const BASE_URL = globalThis.location.origin;

    // Authenticate and get ticket info (including id from raw server response)
    if (globalThis.resetCookieJar) globalThis.resetCookieJar();
    const authRes = await fetch(`${BASE_URL}/authenticate`, {
      method: 'POST',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({
        login: 'bushenevvt',
        password: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
      }),
    });
    const ticket = await authRes.json();
    const ticketId = ticket.id;

    // Create test document
    const testDocUri = 'test55:' + Util.guid();
    // Helper: make request with ticket in query param (no cookies)
    async function fetchWithQueryParam(url, options = {}) {
      const separator = url.includes('?') ? '&' : '?';
      const fullUrl = `${BASE_URL}${url}${separator}ticket=${ticketId}`;
      return nodeFetch(fullUrl, options);
    }

    // Helper: make request with cookie (current session)
    async function fetchWithCookie(url, options = {}) {
      return fetch(`${BASE_URL}${url}`, options);
    }

    const testDoc = {
      '@': testDocUri,
      'rdf:type': Util.newUri('rdfs:Resource'),
      'v-s:author': Util.newUri('td:ValeriyBushenev-Programmer1'),
      'v-s:created': Util.newDate(new Date()),
      'v-s:test_field': Util.newStr('test055 data', 'EN'),
    };

    // Create test document using Backend (which uses same cookie session)
    const putRes = await Backend.put_individual(testDoc);
    console.log('  put_individual result:', putRes);
    assert(putRes.result === 200, 'Failed to create test document');

    // Wait for modules using Backend
    assert(await Backend.wait_module(Constants.m_acl, putRes.op_id), 'wait_module ACL failed');
    assert(await Backend.wait_module(Constants.m_scripts, putRes.op_id), 'wait_module scripts failed');

    let res;

    console.log('Testing endpoints with cookie and query param...');

    // ===== 1. is_ticket_valid =====
    console.log('  Testing is_ticket_valid...');

    res = await fetchWithCookie('/is_ticket_valid');
    let data = await res.json();
    assert(data === true, 'is_ticket_valid with cookie failed');

    res = await fetchWithQueryParam('/is_ticket_valid');
    data = await res.json();
    assert(data === true, 'is_ticket_valid with query param failed');

    // ===== 2. get_individual =====
    console.log('  Testing get_individual...');

    res = await fetchWithCookie(`/get_individual?uri=${encodeURIComponent(testDocUri)}`);
    console.log('  get_individual cookie status:', res.status, 'uri:', testDocUri);
    if (!res.ok) {
      const errText = await res.text();
      console.log('  get_individual cookie error:', errText);
    }
    assert(res.ok, 'get_individual with cookie failed');
    data = await res.json();
    assert(data['@'] === testDocUri, 'get_individual with cookie returned wrong data');

    res = await fetchWithQueryParam(`/get_individual?uri=${encodeURIComponent(testDocUri)}`);
    assert(res.ok, 'get_individual with query param failed');
    data = await res.json();
    assert(data['@'] === testDocUri, 'get_individual with query param returned wrong data');

    // ===== 3. get_individuals =====
    console.log('  Testing get_individuals...');

    res = await fetchWithCookie('/get_individuals', {
      method: 'POST',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({uris: [testDocUri]}),
    });
    assert(res.ok, 'get_individuals with cookie failed');
    data = await res.json();
    assert(Array.isArray(data) && data.length === 1, 'get_individuals with cookie returned wrong data');

    res = await fetchWithQueryParam('/get_individuals', {
      method: 'POST',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({uris: [testDocUri]}),
    });
    assert(res.ok, 'get_individuals with query param failed');
    data = await res.json();
    assert(Array.isArray(data) && data.length === 1, 'get_individuals with query param returned wrong data');

    // ===== 4. get_rights =====
    console.log('  Testing get_rights...');

    res = await fetchWithCookie(`/get_rights?uri=${encodeURIComponent(testDocUri)}`);
    assert(res.ok, 'get_rights with cookie failed');
    data = await res.json();
    assert('v-s:canRead' in data, 'get_rights with cookie returned no rights');

    res = await fetchWithQueryParam(`/get_rights?uri=${encodeURIComponent(testDocUri)}`);
    assert(res.ok, 'get_rights with query param failed');
    data = await res.json();
    assert('v-s:canRead' in data, 'get_rights with query param returned no rights');

    // ===== 5. query (GET) =====
    console.log('  Testing query (GET)...');

    // Wait for fulltext indexer before query tests
    await Backend.wait_module(Constants.m_fulltext_indexer, putRes.op_id);

    const queryStr = `'@'=='${testDocUri}'`;
    res = await fetchWithCookie(`/query?query=${encodeURIComponent(queryStr)}`);
    assert(res.ok, 'query GET with cookie failed');
    data = await res.json();
    assert(data.result && data.result.includes(testDocUri), 'query GET with cookie returned wrong data');

    res = await fetchWithQueryParam(`/query?query=${encodeURIComponent(queryStr)}`);
    assert(res.ok, 'query GET with query param failed');
    data = await res.json();
    assert(data.result && data.result.includes(testDocUri), 'query GET with query param returned wrong data');

    // ===== 6. query (POST) =====
    console.log('  Testing query (POST)...');

    res = await fetchWithCookie('/query', {
      method: 'POST',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({query: queryStr}),
    });
    assert(res.ok, 'query POST with cookie failed');
    data = await res.json();
    assert(data.result && data.result.includes(testDocUri), 'query POST with cookie returned wrong data');

    res = await fetchWithQueryParam('/query', {
      method: 'POST',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({query: queryStr}),
    });
    assert(res.ok, 'query POST with query param failed');
    data = await res.json();
    assert(data.result && data.result.includes(testDocUri), 'query POST with query param returned wrong data');

    // ===== 7. put_individual (via set_in_individual) =====
    console.log('  Testing set_in_individual...');

    const updateDoc = {
      '@': testDocUri,
      'v-s:test_field': Util.newStr('updated via cookie', 'EN'),
    };

    res = await fetchWithCookie('/set_in_individual', {
      method: 'PUT',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({individual: updateDoc, prepare_events: true}),
    });
    assert(res.ok, 'set_in_individual with cookie failed');

    updateDoc['v-s:test_field'] = Util.newStr('updated via query param', 'EN');
    res = await fetchWithQueryParam('/set_in_individual', {
      method: 'PUT',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({individual: updateDoc, prepare_events: true}),
    });
    assert(res.ok, 'set_in_individual with query param failed');

    // ===== 8. add_to_individual =====
    console.log('  Testing add_to_individual...');

    const addDoc = {
      '@': testDocUri,
      'v-s:test_field2': Util.newStr('added via cookie', 'EN'),
    };

    res = await fetchWithCookie('/add_to_individual', {
      method: 'PUT',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({individual: addDoc, prepare_events: true}),
    });
    assert(res.ok, 'add_to_individual with cookie failed');

    const addDoc2 = {
      '@': testDocUri,
      'v-s:test_field3': Util.newStr('added via query param', 'EN'),
    };
    res = await fetchWithQueryParam('/add_to_individual', {
      method: 'PUT',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({individual: addDoc2, prepare_events: true}),
    });
    assert(res.ok, 'add_to_individual with query param failed');

    // ===== 9. remove_from_individual =====
    console.log('  Testing remove_from_individual...');

    const removeDoc = {
      '@': testDocUri,
      'v-s:test_field3': Util.newStr('added via query param', 'EN'),
    };

    res = await fetchWithCookie('/remove_from_individual', {
      method: 'PUT',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({individual: removeDoc, prepare_events: true}),
    });
    assert(res.ok, 'remove_from_individual with cookie failed');

    // ===== 10. get_membership =====
    console.log('  Testing get_membership...');

    const userUri = ticket.user_uri;
    res = await fetchWithCookie(`/get_membership?uri=${encodeURIComponent(userUri)}`);
    assert(res.ok, 'get_membership with cookie failed');

    res = await fetchWithQueryParam(`/get_membership?uri=${encodeURIComponent(userUri)}`);
    assert(res.ok, 'get_membership with query param failed');

    // ===== 11. get_rights_origin =====
    console.log('  Testing get_rights_origin...');

    res = await fetchWithCookie(`/get_rights_origin?uri=${encodeURIComponent(testDocUri)}`);
    assert(res.ok, 'get_rights_origin with cookie failed');

    res = await fetchWithQueryParam(`/get_rights_origin?uri=${encodeURIComponent(testDocUri)}`);
    assert(res.ok, 'get_rights_origin with query param failed');

    // ===== 12. logout =====
    console.log('  Testing logout...');

    // Test logout with cookie - need separate session
    // Authenticate to get fresh cookie and ticket id
    if (globalThis.resetCookieJar) globalThis.resetCookieJar();
    let authRes2 = await fetch(`${BASE_URL}/authenticate`, {
      method: 'POST',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({
        login: 'bushenevvt',
        password: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
      }),
    });
    let logoutTicket = await authRes2.json();

    res = await fetchWithCookie('/logout');
    assert(res.ok, 'logout with cookie failed');
    data = await res.json();
    assert(data.result === 200, 'logout with cookie should return result 200');

    // Check that cookie session is now invalid
    res = await fetchWithCookie('/is_ticket_valid');
    data = await res.json();
    assert(data === false, 'ticket should be invalid after logout with cookie');

    // Test logout with query param - get new ticket
    if (globalThis.resetCookieJar) globalThis.resetCookieJar();
    authRes2 = await fetch(`${BASE_URL}/authenticate`, {
      method: 'POST',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({
        login: 'bushenevvt',
        password: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
      }),
    });
    logoutTicket = await authRes2.json();
    const logoutTicketId = logoutTicket.id;

    // Clear cookies and use query param
    globalThis.resetCookieJar();
    res = await nodeFetch(`${BASE_URL}/logout?ticket=${logoutTicketId}`);
    assert(res.ok, 'logout with query param failed');
    data = await res.json();
    assert(data.result === 200, 'logout with query param should return result 200');

    // Check that ticket is now invalid
    res = await nodeFetch(`${BASE_URL}/is_ticket_valid?ticket=${logoutTicketId}`);
    data = await res.json();
    assert(data === false, 'ticket should be invalid after logout with query param');

    // ===== Cleanup =====
    console.log('  Cleaning up...');

    // Re-authenticate to restore session
    if (globalThis.resetCookieJar) globalThis.resetCookieJar();
    await Backend.authenticate('bushenevvt', 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');

    // Remove test document
    await Backend.remove_individual(testDocUri);

    console.log('All endpoints passed!');
  });
};
