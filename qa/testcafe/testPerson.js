import Basic from './basic'
import { Selector, t } from 'testcafe';
  fixture `test Person`
    .page `http://localhost:8080/`;
  const basic = new Basic();
  test('testPerson', async t => {
    basic.login('karpovrt', '123');
    const timeStamp = ''+Math.round(+new Date()/1000);
    basic.createTestUI('Вася Пупкин', timeStamp);
    basic.attributiveSearchTestUi(timeStamp);
    await t
      .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('1')
});
