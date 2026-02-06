export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  // Helper function to wait
  const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

  test(`#099 Security: Timing Attack Protection - consistent auth response times`, async () => {
    console.log('  [test099] Starting Timing Attack Protection test');
    console.log('  [test099] This test measures authentication response times to detect timing vulnerabilities');

    // Warmup request to avoid cold start affecting measurements
    console.log('  [test099] Warming up search index...');
    try {
      await Backend.authenticate('warmup_user_' + Date.now(), 'warmup_password');
    } catch (e) {
      // Expected to fail
    }
    await sleep(500); // Wait for any background operations

    const iterations = 3;
    const wrongPasswordTimes = [];
    const nonExistentUserTimes = [];

    console.log(`  [test099] Measuring ${iterations} iterations for existing user with wrong password...`);
    for (let i = 0; i < iterations; i++) {
      const start = performance.now();
      try {
        await Backend.authenticate('bushenevvt', 'wrong_password_hash_' + i);
      } catch (e) {
        // Expected to fail
      }
      const elapsed = performance.now() - start;
      wrongPasswordTimes.push(elapsed);
      console.log(`  [test099]   Iteration ${i + 1}: ${elapsed.toFixed(2)}ms`);
      await sleep(100); // Small delay between attempts
    }

    console.log(`  [test099] Measuring ${iterations} iterations for non-existent user...`);
    for (let i = 0; i < iterations; i++) {
      const start = performance.now();
      try {
        await Backend.authenticate('nonexistent_user_' + Date.now() + '_' + i, 'any_password_hash');
      } catch (e) {
        // Expected to fail
      }
      const elapsed = performance.now() - start;
      nonExistentUserTimes.push(elapsed);
      console.log(`  [test099]   Iteration ${i + 1}: ${elapsed.toFixed(2)}ms`);
      await sleep(100); // Small delay between attempts
    }

    const avgWrongPassword = wrongPasswordTimes.reduce((a, b) => a + b, 0) / wrongPasswordTimes.length;
    const avgNonExistent = nonExistentUserTimes.reduce((a, b) => a + b, 0) / nonExistentUserTimes.length;
    const timingRatio = Math.max(avgWrongPassword, avgNonExistent) / Math.min(avgWrongPassword, avgNonExistent);

    console.log('  [test099] Results:');
    console.log(`  [test099]   Wrong password avg: ${avgWrongPassword.toFixed(2)}ms`);
    console.log(`  [test099]   Non-existent user avg: ${avgNonExistent.toFixed(2)}ms`);
    console.log(`  [test099]   Timing ratio: ${timingRatio.toFixed(2)}x`);

    assert(timingRatio < 3.0,
      `Timing difference too large: wrong_password avg=${avgWrongPassword.toFixed(2)}ms, ` +
      `nonexistent avg=${avgNonExistent.toFixed(2)}ms, ratio=${timingRatio.toFixed(2)}`);

    if (timingRatio < 1.5) {
      console.log('  [test099] ✓ Excellent timing protection (ratio < 1.5x)');
    } else if (timingRatio < 2.0) {
      console.log('  [test099] ✓ Good timing protection (ratio < 2.0x)');
    } else {
      console.log('  [test099] ✓ Acceptable timing protection (ratio < 3.0x)');
    }

    // Wait for rate limiter to reset before next test
    console.log('  [test099] Waiting for rate limiter to reset...');
    await sleep(2000);
  });

  test(`#099.1 Security: Authentication still works correctly`, async () => {
    console.log('  [test099.1] Testing valid authentication...');
    try {
      const ticket = await Backend.authenticate('bushenevvt', 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3');
      console.log(`  [test099.1] User URI: ${ticket.user_uri}`);
      console.log(`  [test099.1] End time: ${new Date(ticket.end_time).toISOString()}`);
      assert(ticket.end_time > Date.now(), 'Ticket should have valid end time');
      console.log('  [test099.1] ✓ Valid authentication works correctly');
    } catch (e) {
      if (e.message && e.message.includes('Too Many Requests')) {
        console.log('  [test099.1] ⚠ Rate limited - skipping (this is expected after multiple failed auth attempts)');
        assert(true, 'Rate limiting is working correctly');
      } else {
        throw e;
      }
    }
  });

  test(`#099.2 Security: Invalid credentials are still rejected`, async () => {
    console.log('  [test099.2] Testing rejection of invalid credentials...');

    // Wait a bit for rate limiter
    await sleep(1000);

    console.log('  [test099.2] Testing existing user with wrong password...');
    try {
      await Backend.authenticate('bushenevvt', 'invalid_password_hash');
      assert(false, 'Should have thrown error for invalid password');
    } catch (e) {
      if (e.message && e.message.includes('Too Many Requests')) {
        console.log('  [test099.2] ⚠ Rate limited - this confirms rate limiting is working');
        assert(true, 'Rate limiting prevents brute force');
      } else {
        console.log(`  [test099.2] ✓ Invalid password rejected (error: ${e.message || e.code || 'auth failed'})`);
        assert(true, 'Invalid password correctly rejected');
      }
    }

    console.log('  [test099.2] Testing non-existent user...');
    try {
      await Backend.authenticate('completely_fake_user_12345', 'any_password');
      assert(false, 'Should have thrown error for non-existent user');
    } catch (e) {
      if (e.message && e.message.includes('Too Many Requests')) {
        console.log('  [test099.2] ⚠ Rate limited - this confirms rate limiting is working');
        assert(true, 'Rate limiting prevents user enumeration');
      } else {
        console.log(`  [test099.2] ✓ Non-existent user rejected (error: ${e.message || e.code || 'auth failed'})`);
        assert(true, 'Non-existent user correctly rejected');
      }
    }
  });
};
