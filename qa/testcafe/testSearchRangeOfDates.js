import Basic from './basic'
import { Selector, t } from 'testcafe';
  fixture `test Search Range Of Dates`
    .page `http://localhost:8080/`
  const basic = new Basic();
  const birthDate =  '01.01.2014';
  const birthDate1 = '02.12.2015';
  const birthDate2 = '12.07.2016';
  const query = '\'v-s:birthday\' == [2014-01-01T00:00:00, 2014-01-01T23:59:59] && \'v-s:middleName\' == \'DatesO\'';
  const query1 = '\'v-s:birthday\' == [2014-01-01T00:00:00, 2015-12-02T23:59:59] && \'v-s:middleName\' == \'DatesO\'';
  const query2 = '\'v-s:birthday\' == [2014-01-01T00:00:00, 2016-07-12T23:59:59] && \'v-s:middleName\' == \'DatesO\'';
  const query3 = '\'v-s:birthday\' == [2014-01-01T00:00:00, 2034-07-12T23:59:59] && \'v-s:middleName\' == \'DatesO\'';
  test('testSearchRangeOfDates', async t => {
    basic.login('karpovrt', '123');
    await t
      .click('#menu')
      .click('li[id="menu"] li[resource="v-l:Create"]')
      .typeText('veda-control.fulltext.dropdown', 'Персона')
      .click('div.suggestion[about="v-s:Person"]')

      .typeText('veda-control.-view.edit.search[property="v-s:lastName"]', 'Range')
      .typeText('veda-control.-view.edit.search.has-error[property="v-s:firstName"]', 'Of')
      .typeText('veda-control.-view.edit.search[property="v-s:middleName"]', 'DatesO')
      .click('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]')
      .pressKey('ctrl+a delete')
      .typeText('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]', birthDate)
      .click('veda-control.-view.edit.search[property="v-s:lastName"]')
      .click('span[data-template="v-ui:StandardButtonsTemplate"] span[typeof="v-s:Person"] button#save')

      .click('#menu')
      .click('li[id="menu"] li[resource="v-l:Create"]')
      .typeText('veda-control.-view.edit.search[property="v-s:lastName"]', 'Range')
      .typeText('veda-control.-view.edit.search.has-error[property="v-s:firstName"]', 'Of')
      .typeText('veda-control.-view.edit.search[property="v-s:middleName"]', 'DatesO')
      .click('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]')
      .pressKey('ctrl+a delete')
      .typeText('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]', birthDate1)
      .click('veda-control.-view.edit.search[property="v-s:lastName"]')
      .click('span[data-template="v-ui:StandardButtonsTemplate"] span[typeof="v-s:Person"] button#save')

      .click('#menu')
      .click('li[id="menu"] li[resource="v-l:Create"]')
      .typeText('veda-control.-view.edit.search[property="v-s:lastName"]', 'Range')
      .typeText('veda-control.-view.edit.search.has-error[property="v-s:firstName"]', 'Of')
      .typeText('veda-control.-view.edit.search[property="v-s:middleName"]', 'DatesO')
      .click('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]')
      .pressKey('ctrl+a delete')
      .typeText('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]', birthDate2)
      .click('veda-control.-view.edit.search[property="v-s:lastName"]')
      .click('span[data-template="v-ui:StandardButtonsTemplate"] span[typeof="v-s:Person"] button#save')

      .click('#menu')
      .click('li[id="menu"] li[resource="v-l:Create"]')
      .typeText('veda-control.-view.edit.search[property="v-s:lastName"]', 'Range')
      .typeText('veda-control.-view.edit.search.has-error[property="v-s:firstName"]', 'Of')
      .typeText('veda-control.-view.edit.search[property="v-s:middleName"]', 'DatesO')
      .click('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]')
      .click('veda-control.-view.edit.search[property="v-s:lastName"]')
      .click('span[data-template="v-ui:StandardButtonsTemplate"] span[typeof="v-s:Person"] button#save')

      .click('ul.nav.navbar-nav.navbar-right li[about="v-fs:FulltextSearch"]')
      .typeText('div.input-group input[name="v_s_userthing_*"]', query)
      .click('span.input-group-btn button[about="v-fs:Find"]')
      .expect(Selector('small.stats-top.pull-right span.badge[property="v-fs:authorized"]').innerText).eql('1')

      .click('div.input-group input[name="v_s_userthing_*"]')
      .pressKey('ctrl+a delete')
      .typeText('div.input-group input[name="v_s_userthing_*"]', query1)
      .click('span.input-group-btn button[about="v-fs:Find"]')
      .expect(Selector('small.stats-top.pull-right span.badge[property="v-fs:authorized"]').innerText).eql('2')

      .click('div.input-group input[name="v_s_userthing_*"]')
      .pressKey('ctrl+a delete')
      .typeText('div.input-group input[name="v_s_userthing_*"]', query2)
      .click('span.input-group-btn button[about="v-fs:Find"]')
      .expect(Selector('small.stats-top.pull-right span.badge[property="v-fs:authorized"]').innerText).eql('3')

      .click('div.input-group input[name="v_s_userthing_*"]')
      .pressKey('ctrl+a delete')
      .typeText('div.input-group input[name="v_s_userthing_*"]', query3)
      .click('span.input-group-btn button[about="v-fs:Find"]')
      .expect(Selector('small.stats-top.pull-right span.badge[property="v-fs:authorized"]').innerText).eql('4')
});


