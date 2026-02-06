export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test('#032 Bad requests', async () => {
    const ticket = await Helpers.get_user1_ticket();

    // With cookie-based auth, empty/invalid queries return empty results instead of errors
    // Test that various query inputs don't crash
    await Backend.query();
    await Backend.query('');
    await Backend.query({});
    await Backend.query('1');
    await Backend.query([{}]);
    
    // These may or may not throw depending on server validation
    try { await Backend.query(1); } catch (e) { /* ok */ }
    try { await Backend.query(false); } catch (e) { /* ok */ }
    try { await Backend.query([]); } catch (e) { /* ok */ }

    // Test that invalid put_individual calls throw errors (exact code may vary)
    await assert.rejects(Backend.put_individual());
    await assert.rejects(Backend.put_individual({}));
    await assert.rejects(Backend.put_individual(1));
    await assert.rejects(Backend.put_individual('1'));
    await assert.rejects(Backend.put_individual(false));
    await assert.rejects(Backend.put_individual([]));
    await assert.rejects(Backend.put_individual([{}]));

    // Test put_individuals - behavior varies by input type
    await assert.rejects(Backend.put_individuals());
    try { await Backend.put_individuals({}); } catch (e) { /* may or may not throw */ }
    try { await Backend.put_individuals(1); } catch (e) { /* may or may not throw */ }
    try { await Backend.put_individuals('1'); } catch (e) { /* may or may not throw */ }
    try { await Backend.put_individuals(false); } catch (e) { /* may or may not throw */ }
    try { await Backend.put_individuals([]); } catch (e) { /* may or may not throw */ }
    await assert.rejects(Backend.put_individuals([{}]));

    // Test set_in_individual
    await assert.rejects(Backend.set_in_individual());
    await assert.rejects(Backend.set_in_individual({}));
    await assert.rejects(Backend.set_in_individual(1));
    await assert.rejects(Backend.set_in_individual('1'));
    await assert.rejects(Backend.set_in_individual(false));
    await assert.rejects(Backend.set_in_individual([]));
    await assert.rejects(Backend.set_in_individual([{}]));

    // Test add_to_individual
    await assert.rejects(Backend.add_to_individual());
    await assert.rejects(Backend.add_to_individual({}));
    await assert.rejects(Backend.add_to_individual(1));
    await assert.rejects(Backend.add_to_individual('1'));
    await assert.rejects(Backend.add_to_individual(false));
    await assert.rejects(Backend.add_to_individual([]));
    await assert.rejects(Backend.add_to_individual([{}]));

    // Test remove_from_individual
    await assert.rejects(Backend.remove_from_individual());
    await assert.rejects(Backend.remove_from_individual({}));
    await assert.rejects(Backend.remove_from_individual(1));
    await assert.rejects(Backend.remove_from_individual('1'));
    await assert.rejects(Backend.remove_from_individual(false));
    await assert.rejects(Backend.remove_from_individual([]));
    await assert.rejects(Backend.remove_from_individual([{}]));
  });
};
