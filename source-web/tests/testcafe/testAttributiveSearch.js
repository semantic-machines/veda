import Basic from './basic';
import config from './config';
import {Selector} from 'testcafe';
fixture `test Attributive Search`
  .page `${config.baseUrl}`;
const basic = new Basic();
const pageForNavigateFromConfig = `${config.baseUrl}`+'#/v-ui:TestUIRegistry';
const first = (new Date%9e6).toString(36);
const last = (new Date%9e6).toString(36);
const middle = (new Date%9e6).toString(36);
const birth = '01.01.'+Math.floor(1000 + Math.random() * 9000);
test('testAttributiveSearch', async (t) => {
  const username = 'karpovrt';
  const password = '123';
  console.log(`Шаг 1: Login as ${username}`);
  basic.login(username, password);
  
  console.log(`Шаг 2: Create first test UI (${last+'b'}, ${first+'cbb'}, ${middle+'Q'}, ${birth})`);
  basic.createTestUiForAttributiveSearch(last+'b', first+'cbb', middle+'Q', birth);
  
  console.log(`Шаг 3: Create second test UI (a${last}, ${first+'bcc'}, T${middle}, ${birth})`);
  basic.createTestUiForAttributiveSearch('a'+last, first+'bcc', 'T'+middle, birth);
  
  console.log('Шаг 4: Navigate to test registry and verify user');
  await t
    .expect(Selector('#user-info').innerText).contains('Администратор2 .')
    .navigateTo( pageForNavigateFromConfig );
  
  console.log(`Шаг 5: Search by label 'a${last}' - expect 1 result`);
  await t
    .typeText('veda-control#label', 'a' + last)
    .click('veda-control#comment')
    .click('button#search-button')
    .expect(Selector('.stats-top span[property="v-fs:authorized"]').innerText).contains('1');
  
  console.log(`Шаг 6: Search by comment '${first}' - expect 2 results`);
  await t
    .click('veda-control#label')
    .pressKey('ctrl+a delete')
    .typeText('veda-control#comment', first)
    .click('button#search-button')
    .expect(Selector('.stats-top span[property="v-fs:authorized"]').innerText).contains('2');
  
  console.log(`Шаг 7: Search by comment '${first}ccc' - expect 0 results`);
  await t
    .click('veda-control#comment')
    .pressKey('ctrl+a delete')
    .typeText('veda-control#comment', first + 'ccc')
    .click('button#search-button')
    .expect(Selector('.stats-top span[property="v-fs:authorized"]').innerText).contains('0');
  
  console.log(`Шаг 8: Search by testString '${middle}Q' - expect 1 result`);
  await t
    .click('veda-control#comment')
    .pressKey('ctrl+a delete')
    .typeText('veda-control#testString', middle+'Q')
    .click('veda-control#comment')
    .click('button#search-button')
    .expect(Selector('.stats-top span[property="v-fs:authorized"]').innerText).contains('1');
  
  console.log(`Шаг 9: Search by birth date '${birth}' - expect 2 results`);
  await t
    .click('veda-control#testString')
    .pressKey('ctrl+a delete')
    .hover('veda-control[rel="v-ui:testLink"]')
    .click('veda-control#date')
    .pressKey('ctrl+a delete')
    .typeText('veda-control[property="v-ui:testDatetime"]#date', birth)
    .click('veda-control#comment')
    .click('button#search-button')
    .expect(Selector('.stats-top span[property="v-fs:authorized"]').innerText).contains('2');
  
  console.log(`Шаг 10: Search by birth date '${birth}' and comment '${first}' - expect 2 results`);
  await t
    .hover('veda-control[rel="v-ui:testLink"]')
    .click('veda-control#date')
    .pressKey('ctrl+a delete')
    .typeText('veda-control[property="v-ui:testDatetime"]#date', birth)
    .click('veda-control#label')
    .click('veda-control#comment')
    .pressKey('ctrl+a delete')
    .typeText('veda-control#comment', first)
    .click('button#search-button')
    .expect(Selector('.stats-top span[property="v-fs:authorized"]').innerText).contains('2');
  
  console.log(`Шаг 11: Complex search by multiple fields - expect 1 result`);
  await t
    .hover('veda-control[rel="v-ui:testLink"]')
    .click('veda-control#date')
    .pressKey('ctrl+a delete')
    .typeText('veda-control[property="v-ui:testDatetime"]#date', birth)
    .click('veda-control#label')
    .click('veda-control#comment')
    .pressKey('ctrl+a delete')
    .typeText('veda-control#comment', first)
    .typeText('veda-control#label', last)
    .typeText('veda-control#testString', middle+'Q')
    .click('veda-control#label')
    .click('button#search-button')
    .expect(Selector('.stats-top span[property="v-fs:authorized"]').innerText).contains('1');
  
  console.log('Тест testAttributiveSearch завершён');
});
test('testSearchOrderBy', async (t) => {
  const username = 'karpovrt';
  const password = '123';
  console.log(`Шаг 1: Login as ${username}`);
  basic.login(username, password);
  
  console.log('Шаг 2: Navigate to test registry and verify user');
  await t
    .expect(Selector('#user-info').innerText).contains('Администратор2 .')
    .navigateTo( pageForNavigateFromConfig );
  
  console.log('Шаг 3: Click search button and wait');
  await t
    .click('button#search-button')
    .wait(1000);
  
  console.log('Шаг 4: Order by testInteger descending - expect 999');
  await t
    .click('div.results div.search-result.table-responsive.noSwipe [data-orderby="v-ui:testInteger"] > a.glyphicon')
    .expect(Selector('div.results div.search-result.table-responsive.noSwipe tbody.result-container td[property="v-ui:testInteger"]').innerText).eql('999')
    .wait(1000);
  
  console.log('Шаг 5: Order by testInteger ascending - expect 2');
  await t
    .click('div.results div.search-result.table-responsive.noSwipe [data-orderby="v-ui:testInteger"] > a.glyphicon')
    .expect(Selector('div.results div.search-result.table-responsive.noSwipe tbody.result-container td[property="v-ui:testInteger"]').innerText).eql('2');
  
  console.log('Тест testSearchOrderBy завершён');
});
