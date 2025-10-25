import Basic from './basic';
import config from './config';
import {Selector} from 'testcafe';
fixture `test Person`
  .page `${config.baseUrl}`;
const basic = new Basic();
const pageForNavigateFromConfig = `${config.baseUrl}#/v-ui:TestUIRegistry`;
test('testPerson', async (t) => {
  const username = 'karpovrt';
  const password = '123';
  const testLabel = 'Вася Пупкин';
  const expectedResults = '1';
  
  console.log(`Шаг 1: Login as ${username}`);
  basic.login(username, password);
  
  const timeStamp = Math.round(Date.now() / 1000).toString();
  
  console.log(`Шаг 2: Create test UI (${testLabel}, ${timeStamp})`);
  basic.createTestUI(testLabel, timeStamp);
  
  console.log('Шаг 3: Navigate to test registry and verify user');
  await t
    .expect(Selector('#user-info').innerText).contains('Администратор2 .')
    .navigateTo( pageForNavigateFromConfig )
    .wait(2000);
  
  console.log(`Шаг 4: Search for ${timeStamp} - expect ${expectedResults} result`);
  await t
    .typeText('veda-control#comment', timeStamp)
    .click('button#search-button')
    .expect(Selector('.stats-top span[property="v-fs:authorized"]').innerText).eql(expectedResults);
  
  console.log('Тест testPerson завершён');
});
