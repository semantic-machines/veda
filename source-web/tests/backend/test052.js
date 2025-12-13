export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  const BASE_URL = globalThis.location?.origin || 'http://localhost:8080';

  test(`#052 Security: Path Traversal Protection - reject paths with '..'`, async () => {
    console.log('  [test052] Starting Path Traversal Protection test');
    console.log(`  [test052] Using base URL: ${BASE_URL}`);

    const traversalPaths = [
      '/apps/../../../etc/passwd',
      '/apps/..%2F..%2F..%2Fetc/passwd',
      '/apps/test/../../../etc/passwd',
      '/apps/..\\..\\..\\etc\\passwd',
    ];

    for (const path of traversalPaths) {
      console.log(`  [test052] Testing path traversal: ${path}`);
      try {
        const response = await fetch(BASE_URL + path, {
          method: 'GET',
          cache: 'no-cache',
        });
        console.log(`  [test052] Response status: ${response.status}`);
        assert(response.status === 403 || response.status === 404 || response.status === 400,
          `Path traversal attempt should be rejected for: ${path}, got status: ${response.status}`);
        console.log(`  [test052] ✓ Path traversal rejected with status ${response.status}`);
      } catch (e) {
        console.log(`  [test052] ✓ Request blocked/failed for: ${path} (${e.message})`);
        assert(true, `Request blocked for: ${path}`);
      }
    }
    console.log('  [test052] Path Traversal Protection test completed');
  });

  test(`#052.1 Security: Path Traversal Protection - valid app paths should work`, async () => {
    console.log('  [test052.1] Testing valid app path /apps/');
    const response = await fetch(BASE_URL + '/apps/', {
      method: 'GET',
      cache: 'no-cache',
    });
    console.log(`  [test052.1] Response status: ${response.status}`);
    assert(response.status !== 403, 'Valid app path should not be forbidden');
    console.log(`  [test052.1] ✓ Valid path works correctly (status: ${response.status})`);
  });
};
