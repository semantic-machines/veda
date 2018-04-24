import Basic from './basic'
import { Selector, t } from 'testcafe';
  fixture `test Rights`
    .page `http://localhost:8080/`
  const basic = new Basic();
  test('testRights', async t => {
    basic.login('karpovrt', '123');
    const timeEdwin = ''+Math.round(+new Date()/1000);
    const timeFillips = ''+Math.round(+new Date()/100);
    basic.createPerson('Кинг', 'Стивен', timeEdwin, '21.09.1947');
    basic.logout();
    basic.login('bychinat', '123');
    basic.fullTextSearch(timeEdwin, '0');
    basic.createPerson('Лавкрафт', 'Говард', timeFillips, '20.08.1890');
    basic.logout();
    basic.login('karpovrt', '123');
    basic.fullTextSearch(timeFillips, '1');
    await t
      .click('tbody.result-container a.glyphicon.glyphicon-search')
      .expect(Selector('div.col-md-9 h3 span[property="v-s:lastName"]').innerText).eql('Лавкрафт')
      .expect(Selector('div.col-md-9 h3 span[property="v-s:firstName"]').innerText).eql('Говард')
      .expect(Selector('div.col-md-9 h3 span[property="v-s:middleName"]').innerText).eql(timeFillips)
  });
