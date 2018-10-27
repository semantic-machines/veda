import Basic from './basic'
import { Selector, t } from 'testcafe';
  fixture `test Attributive Search`
    .page `http://localhost:8080/`
  const basic = new Basic();
  const first =  'xGIo5f';
  const last = 'GhiOJe';
  const middle = 'NE1UCD';
  const birth = '01.01.1990';
  test('testAttributiveSearch', async t => {
    basic.login('karpovrt', '123');
    basic.createTestUiForAttributiveSearch(last+'b', first+'cbb', middle+'Q', birth);
    basic.createTestUiForAttributiveSearch('a'+last, first+'bcc', 'T'+middle, birth);
    await t
      .expect(Selector('#user-info').innerText).eql('Администратор2\n')
      .navigateTo('http://localhost:8080/#/v-ui:TestUIRegistry')
      .typeText('veda-control#label', 'a' + last)
      .click('veda-control#comment')
      .click('button#search-button')
      .expect(Selector('h3.clearfix span[property="v-fs:authorized"]').innerText).eql('1')
      
      .click('veda-control#label')
      .pressKey('ctrl+a delete')
      .typeText('veda-control#comment', first.substring(0,4) + '*')
      .click('button#search-button')
      .expect(Selector('h3.clearfix span[property="v-fs:authorized"]').innerText).eql('2')

      .click('veda-control#comment')
      .pressKey('ctrl+a delete')
      .typeText('veda-control#comment', first + 'ccc')
      .click('button#search-button')
      .expect(Selector('h3.clearfix span[property="v-fs:authorized"]').innerText).eql('0')

      .click('veda-control#comment')
      .pressKey('ctrl+a delete')
      .typeText('veda-control#testString', middle)
      .click('veda-control#comment')
      .click('button#search-button')
      .expect(Selector('h3.clearfix span[property="v-fs:authorized"]').innerText).eql('1')
      
      .click('veda-control#testString')
      .pressKey('ctrl+a delete')
      .click('veda-control#date')
      .pressKey('ctrl+a delete')
      .typeText('veda-control[property="v-ui:testDatetime"]#date', birth)
      .click('veda-control#comment')
      .click('button#search-button')
      .expect(Selector('h3.clearfix span[property="v-fs:authorized"]').innerText).eql('2')

      .click('veda-control#date')
      .pressKey('ctrl+a delete')
      .typeText('veda-control[property="v-ui:testDatetime"]#date', birth)
      .click('veda-control#label')
      .click('veda-control#comment')
      .pressKey('ctrl+a delete')
      .typeText('veda-control#comment', first)
      .click('button#search-button')
      .expect(Selector('h3.clearfix span[property="v-fs:authorized"]').innerText).eql('2')

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
      .expect(Selector('h3.clearfix span[property="v-fs:authorized"]').innerText).eql('1');
  });
