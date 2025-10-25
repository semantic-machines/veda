import Basic from './basic';
import config from './config';
import {Selector} from 'testcafe';
fixture `test language`
  .page `${config.baseUrl}`;
const basic = new Basic();
test('testLanguage', async (t) => {
  const username = 'karpovrt';
  const password = '123';
  const expectedEN = 'Administrator2 .';
  const expectedRU = 'Администратор2 .';
  
  console.log(`Шаг 1: Login as ${username}`);
  basic.login(username, password);
  
  console.log('Шаг 2: Switch to EN and verify');
  await t
    .click('button[about="v-ui:EN"]')
    .click('button[about="v-ui:RU"]')
    .expect(Selector('#user-info').innerText).contains(expectedEN);
  
  console.log('Шаг 3: Switch to RU and verify');
  await t
    .click('button[about="v-ui:RU"]')
    .click('button[about="v-ui:EN"]')
    .expect(Selector('#user-info').innerText).contains(expectedRU);
  
  console.log('Тест testLanguage завершён');
});
