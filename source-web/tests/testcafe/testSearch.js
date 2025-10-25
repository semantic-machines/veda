import Basic from './basic';
import config from './config';
import {Selector} from 'testcafe';
fixture `test search`
  .page `${config.baseUrl}`;
const basic = new Basic();
const pageForNavigateFromConfig = `${config.baseUrl}`+'#/v-ui:TestUIRegistry';
test('testSearch', async (t) => {
  const username = 'karpovrt';
  const password = '123';
  
  console.log(`Шаг 1: Login as ${username}`);
  basic.login(username, password);
  
  console.log('Шаг 2: Navigate to test registry and verify user');
  await t
    .expect(Selector('#user-info').innerText).contains('Администратор2 .')
    .navigateTo( pageForNavigateFromConfig );
  
  console.log('Шаг 3: Click search button and wait for results');
  await t
    .click('button#search-button')
    .wait(5000);
  
  const search = Selector('div.results div.search-result.table-responsive.noSwipe tbody.result-container tr[typeof="v-ui:TestUIClass"]');
  const count = await search.count;
  
  console.log(`Шаг 4: Verify search results count - expect ${count}`);
  await t
    .expect(Selector('.stats-top span[property="v-fs:authorized"]').innerText).eql(''+count);
  
  console.log('Тест testSearch завершён');
});
