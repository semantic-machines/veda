import Basic from './basic'
import config from './config';
import { Selector, t } from 'testcafe';
  fixture `test Attributive Search`
    .page `${config.baseUrl}`
  const basic = new Basic();
  const first =  (new Date%9e6).toString(36);
  const last = (new Date%9e6).toString(36);
  const middle = (new Date%9e6).toString(36);
  const birth = '01.01.'+Math.floor(1000 + Math.random() * 9000);
  test('testAttributiveSearch', async t => {
    basic.login('karpovrt', '123');
    basic.createTestUiForAttributiveSearch(last+'b', first+'cbb', middle+'Q', birth);
    basic.createTestUiForAttributiveSearch('a'+last, first+'bcc', 'T'+middle, birth);
    await t
      .expect(Selector('#user-info').innerText).eql('Администратор2 .\n')
      .navigateTo('http://localhost:8080/#/v-ui:TestUIRegistry')
      .typeText('veda-control#label', 'a' + last)
      .click('veda-control#comment')
      .click('button#search-button')
      .expect(Selector('.stats-top span[property="v-fs:authorized"]').innerText).eql('1')

      .click('veda-control#label')
      .pressKey('ctrl+a delete')
      .typeText('veda-control#comment', first.substring(0,4) + '*')
      .click('button#search-button')
      .expect(Selector('.stats-top span[property="v-fs:authorized"]').innerText).eql('2')

      .click('veda-control#comment')
      .pressKey('ctrl+a delete')
      .typeText('veda-control#comment', first + 'ccc')
      .click('button#search-button')
      .expect(Selector('.stats-top span[property="v-fs:authorized"]').innerText).eql('0')

      .click('veda-control#comment')
      .pressKey('ctrl+a delete')
      .typeText('veda-control#testString', middle)
      .click('veda-control#comment')
      .click('button#search-button')
      .expect(Selector('.stats-top span[property="v-fs:authorized"]').innerText).eql('1')

      .click('veda-control#testString')
      .pressKey('ctrl+a delete')
      .click('veda-control#date')
      .pressKey('ctrl+a delete')
      .typeText('veda-control[property="v-ui:testDatetime"]#date', birth)
      .click('veda-control#comment')
      .click('button#search-button')
      .expect(Selector('.stats-top span[property="v-fs:authorized"]').innerText).eql('2')

      .click('veda-control#date')
      .pressKey('ctrl+a delete')
      .typeText('veda-control[property="v-ui:testDatetime"]#date', birth)
      .click('veda-control#label')
      .click('veda-control#comment')
      .pressKey('ctrl+a delete')
      .typeText('veda-control#comment', first)
      .click('button#search-button')
      .expect(Selector('.stats-top span[property="v-fs:authorized"]').innerText).eql('2')

      .click('veda-control#date')
      .pressKey('ctrl+a delete')
      .typeText('veda-control[property="v-ui:testDatetime"]#date', birth)
      .click('veda-control#label')
      .click('veda-control#comment')
      .pressKey('ctrl+a delete')
      .typeText('veda-control#comment', first)
      .typeText('veda-control#label', last)
      .typeText('veda-control#testString', middle)
      .click('veda-control#label')
      .click('button#search-button')
      .expect(Selector('.stats-top span[property="v-fs:authorized"]').innerText).eql('1');
  });
  test('testSearchOrderBy', async t => {
    basic.login('karpovrt', '123');
    await t
      .expect(Selector('#user-info').innerText).eql('Администратор2 .\n')
      .navigateTo('http://localhost:8080/#/v-ui:TestUIRegistry')
      .click('button#search-button')
      .click('div.results div.search-result.table-responsive.noSwipe a.glyphicon.glyphicon-sort-by-attributes')
      .expect(Selector('div.results div.search-result.table-responsive.noSwipe tbody.result-container td[property="v-ui:testInteger"]').innerText).eql('999')
      .click('div.results div.search-result.table-responsive.noSwipe a.glyphicon.glyphicon-sort-by-attributes')
      .expect(Selector('div.results div.search-result.table-responsive.noSwipe tbody.result-container td[property="v-ui:testInteger"]').innerText).eql('2')
  });
