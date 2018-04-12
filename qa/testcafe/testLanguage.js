import Basic from './basic'
import { Selector, t } from 'testcafe';
  fixture `test comment`
  .page `http://localhost:8080/`
  const basic = new Basic();
  test('testLanguage', async t => {
    basic.login('karpovrt', '123');
    await t
      //EN
      .click('button[about="v-ui:EN"]')
      .click('button[about="v-ui:RU"]')
      .expect(Selector('#user-info').innerText).eql('Administrator2\n')
      //RU
      .click('button[about="v-ui:RU"]')
      .click('button[about="v-ui:EN"]')
      .expect(Selector('#user-info').innerText).eql('Администратор2\n');
  });


