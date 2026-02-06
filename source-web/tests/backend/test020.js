export default ({test, assert, Backend, Helpers, Constants, Util}) => {
  test(`#020 Search with cursor`, async () => {
    // Login as user1
    const user = await Helpers.get_user1_ticket();

    const testId = Util.guid().substring(0, 8);
    const meeting_template = `{
      "@": "d:QueryTestResource_${testId}_$i",
      "rdf:type": [{ "type": "Uri", "data": "rdfs:Resource" }],
      "v-s:creator": [{ "type": "Uri", "data": "$creator" }],
      "rdfs:label": [{ "type": "String", "data": "$i", "lang": "NONE" }]
    }`;

    await createMeetings(user, 1, 20);
    const res = await createMeetings(user, 21, 1);

    assert(await Backend.wait_module(Constants.m_scripts, res.op_id));
    assert(await Backend.wait_module(Constants.m_acl, res.op_id));
    assert(await Backend.wait_module(Constants.m_fulltext_indexer, res.op_id));

    const q = `'rdf:type'==='rdfs:Resource' && '@'=='d:QueryTestResource_${testId}*'`;
    const s = "'rdfs:label' asc";

    const params1 = {
      query: q,
      sort: s,
      top: 3,
      from: 0,
    };
    const results1 = await Backend.query(params1);
    assert(results1.count === 3 && results1.cursor === 3 && results1.processed === 3);

    const params2 = {
      query: q,
      sort: s,
      top: 10,
      from: 10,
    };
    const results2 = await Backend.query(params2);
    assert(results2.count >= 10 && results2.cursor >= 20 && results2.processed >= 10);

    async function createMeetings (creator, start, count) {
      let result;
      for (let i = start; i < start + count; i++) {
        const meeting = JSON.parse(meeting_template.replace(/\$i/g, i.toString().length === 2 ? i : '0' + i).replace(/\$creator/g, creator.user_uri));
        result = await Backend.put_individual(meeting);
      }
      return result;
    }
  });
};
