import Basic from './basic'
import { Selector, t } from 'testcafe';
  fixture `test Rights`
    .page `http://localhost:8080/`
  const user = 'Стивен Эдвин Кинг';
  const user1 = 'Говард Филлипс Лавкрафт';
  const basic = new Basic();
  test('testRights', async t => {
    basic.login('karpovrt', '123');
    const timeEdwin = ''+Math.round(+new Date()/1000);
    const timeFillips = ''+Math.round(+new Date()/100);
    await t
    .click('#menu')
    .click('li[id="menu"] li[resource="v-l:Create"]')
    .typeText('veda-control.fulltext.dropdown', 'Персона')
    .click('div.suggestion[resource="v-s:Person"]')
    .click('veda-control.-view.edit.search[property="v-s:birthday"]')
    .typeText('veda-control.-view.edit.search[property="v-s:lastName"]', 'Кинг')
    .typeText('veda-control.-view.edit.search.has-error[property="v-s:firstName"]', 'Стивен')
    .typeText('veda-control.-view.edit.search[property="v-s:middleName"]', timeEdwin)
    .wait(1000)
    .click('span[data-template="v-ui:StandardButtonsTemplate"] span[typeof="v-s:Person"] button#save')
    .click('#menu')
    .click('li[id="menu"] li[resource="v-l:Exit"]')
    .click('#login')
    .pressKey('ctrl+a delete')
    .typeText('#login', 'bychinat')
    .click('#submit')
    .expect(Selector('#user-info').innerText).eql('Администратор4\n')
    .click('ul.nav.navbar-nav.navbar-right li[about="v-fs:FulltextSearch"]')
    .typeText('div.input-group input[name="v_s_userthing_*"]', timeEdwin)
    .click('span.input-group-btn button[about="v-fs:Find"]')
    .expect(Selector('div.not-found.alert.alert-warning strong[about="v-fs:Empty"]').innerText).eql('Пусто!')
    .click('#menu')
    .click('li[id="menu"] li[resource="v-l:Create"]')
    .typeText('veda-control.fulltext.dropdown', 'Персона')
    .click('div.suggestion[resource="v-s:Person"]')
    .click('veda-control.-view.edit.search[property="v-s:birthday"]')
    .typeText('veda-control.-view.edit.search[property="v-s:lastName"]', 'Лавкрафт')
    .typeText('veda-control.-view.edit.search.has-error[property="v-s:firstName"]', 'Говард')
    .typeText('veda-control.-view.edit.search[property="v-s:middleName"]', timeFillips)
    .wait(1000)
    .click('span[data-template="v-ui:StandardButtonsTemplate"] span[typeof="v-s:Person"] button#save')
    .click('#menu')
    .click('li[id="menu"] li[resource="v-l:Exit"]')
    .click('#login')
    .pressKey('ctrl+a delete')
    .typeText('#login', 'karpovrt')
    .click('#submit')
    .expect(Selector('#user-info').innerText).eql('Администратор2\n')
    .click('ul.nav.navbar-nav.navbar-right li[about="v-fs:FulltextSearch"]')
    .typeText('div.input-group input[name="v_s_userthing_*"]', timeFillips)
    .click('span.input-group-btn button[about="v-fs:Find"]')
    .wait(2000)
    .click('tbody.result-container a.glyphicon.glyphicon-search')
    .expect(Selector('div.col-md-9 h3 span[property="v-s:lastName"]').innerText).eql('Лавкрафт')
    .expect(Selector('div.col-md-9 h3 span[property="v-s:firstName"]').innerText).eql('Говард')
    .expect(Selector('div.col-md-9 h3 span[property="v-s:middleName"]').innerText).eql(timeFillips)
  });
