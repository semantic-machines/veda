import Basic from './basic';
import config from './config';
import {Selector} from 'testcafe';
fixture `test Search Range Of Dates`
  .page `${config.baseUrl}`;
const basic = new Basic();
test('testSearchRangeOfDates', async (t) => {
  const username = 'karpovrt';
  const password = '123';
  const lastName = 'Range';
  const firstName = 'Of';
  const middleName = 'DatesO';
  const date1 = '01.01.2014';
  const date2 = '02.12.2015';
  const date3 = '12.07.2016';
  const date4 = '12.07.2017';
  const query1 = '\'v-ui:testDatetime\' == [2014-01-01T00:00:00, 2014-01-01T23:59:59] && \'v-ui:testString\' == \'DatesO\'';
  const query2 = '\'v-ui:testDatetime\' == [2014-01-01T00:00:00, 2015-12-02T23:59:59] && \'v-ui:testString\' == \'DatesO\'';
  const query3 = '\'v-ui:testDatetime\' == [2014-01-01T00:00:00, 2016-07-12T23:59:59] && \'v-ui:testString\' == \'DatesO\'';
  const query4 = '\'v-ui:testDatetime\' == [2014-01-01T00:00:00, 2034-07-12T23:59:59] && \'v-ui:testString\' == \'DatesO\'';
  const expected1 = '1';
  const expected2 = '2';
  const expected3 = '3';
  const expected4 = '4';
  
  console.log(`Шаг 1: Login as ${username}`);
  basic.login(username, password);
  
  console.log(`Шаг 2: Create test UI with date ${date1}`);
  basic.createTestUiForAttributiveSearch(lastName, firstName, middleName, date1);
  
  console.log(`Шаг 3: Create test UI with date ${date2}`);
  basic.createTestUiForAttributiveSearch(lastName, firstName, middleName, date2);
  
  console.log(`Шаг 4: Create test UI with date ${date3}`);
  basic.createTestUiForAttributiveSearch(lastName, firstName, middleName, date3);
  
  console.log(`Шаг 5: Create test UI with date ${date4}`);
  basic.createTestUiForAttributiveSearch(lastName, firstName, middleName, date4);
  
  console.log(`Шаг 6: Search for date range 2014 - expect ${expected1} result`);
  basic.fullTextSearch(query1, expected1);
  
  console.log(`Шаг 7: Search for date range 2014-2015 - expect ${expected2} results`);
  basic.fullTextSearch(query2, expected2);
  
  console.log(`Шаг 8: Search for date range 2014-2016 - expect ${expected3} results`);
  basic.fullTextSearch(query3, expected3);
  
  console.log(`Шаг 9: Search for date range 2014-2034 - expect ${expected4} results`);
  basic.fullTextSearch(query4, expected4);
  
  console.log('Шаг 10: Verify user');
  await t
    .expect(Selector('ul.nav.navbar-nav.navbar-right li#user-info').innerText).contains('Администратор2 .');
  
  console.log('Тест testSearchRangeOfDates завершён');
});
