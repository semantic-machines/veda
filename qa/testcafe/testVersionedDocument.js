import Basic from './basic'
import { Selector, t } from 'testcafe';
  fixture `test Versioned Document`
    .page `http://localhost:8080/`
  const basic = new Basic();
  test('testVersionedDocument', async t => {
    basic.login('karpovrt', '123');
    await t
      .click('#menu')
      .click('li[id="menu"] li[resource="v-s:Create"]')
      .typeText('veda-control.fulltext.dropdown', 'Мероприятие')
      .click('div.suggestion[resource="v-s:Action"]')
      .typeText('input.form-control[lang="RU"]', 'Мероприятие')
      .typeText('veda-control[rel="v-s:responsible"] textarea[name="v_s_action_v_s_responsible"]', 'Администратор2')
      .wait(2000)
      .click('div.suggestion[resource="td:RomanKarpov-Analyst1"]')
      .click('#save')
      .click('#edit')
      .wait(2000)
      .typeText('input.form-control[lang="RU"]', '1')
      .click('#save')
      .click('#edit')
      .wait(2000)
      .click('input.form-control[lang="RU"]')
      .pressKey('backspace')
      .typeText('input.form-control[lang="RU"]', '2')
      .click('#save')
      .click('#edit')
      .wait(2000)
      .click('input.form-control[lang="RU"]')
      .pressKey('backspace')
      .typeText('input.form-control[lang="RU"]', '3')
      .click('#save')
      .wait(2000)
      .hover('div[rel="v-s:responsible"]')
      .click('div.col-md-8.col-xs-7.value div.ui-sortable.ui-sortable-disabled[rel="v-s:previousVersion"] a[typeof="v-s:Action v-s:Version"]')
      .expect(Selector('div.col-md-8.col-xs-7 div[property="rdfs:label"] span.value-holder').innerText).eql('Мероприятие3')
      .hover('div[rel="v-s:responsible"]')
      .click('div.col-md-8.col-xs-7.value div.ui-sortable.ui-sortable-disabled[rel="v-s:previousVersion"] a[typeof="v-s:Action v-s:Version"]')
      .expect(Selector('div.col-md-8.col-xs-7 div[property="rdfs:label"] span.value-holder').innerText).eql('Мероприятие2')
      .hover('div[rel="v-s:responsible"]')
      .click('div.col-md-8.col-xs-7.value div.ui-sortable.ui-sortable-disabled[rel="v-s:previousVersion"] a[typeof="v-s:Action v-s:Version"]')
      .expect(Selector('div.col-md-8.col-xs-7 div[property="rdfs:label"] span.value-holder').innerText).eql('Мероприятие1')
      .hover('div[rel="v-s:responsible"]')
      .click('div.col-md-8.col-xs-7.value div.ui-sortable.ui-sortable-disabled[rel="v-s:previousVersion"] a[typeof="v-s:Action v-s:Version"]')
      .expect(Selector('div.col-md-8.col-xs-7 div[property="rdfs:label"] span.value-holder').innerText).eql('Мероприятие')
  });

