import Basic from './basic'
import { Selector, t } from 'testcafe';
  fixture `test Person`
    .page `http://localhost:8080/`;
  const basic = new Basic();
  test('testPerson', async t => {
    basic.login('karpovrt', '123');
    const timeStamp = ''+Math.round(+new Date()/1000);
    basic.createTestUI('Вася Пупкин', timeStamp);
    await t
      .expect(Selector('#user-info').innerText).eql('Администратор2\n')
      .navigateTo('http://localhost:8080/#/v-ui:TestUIRegistry')
      .typeText('veda-control#comment', timeStamp)
      .click('button#search-button')
      .expect(Selector('.stats-top span[property="v-fs:authorized"]').innerText).eql('1')
});
