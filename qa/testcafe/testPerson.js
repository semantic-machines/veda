import Basic from './basic'
import { Selector, t } from 'testcafe';
  fixture `test Person`
    .page `http://localhost:8080/`;
  const basic = new Basic();
  test('testPerson', async t => {
    basic.login('karpovrt', '123');
    const timeStamp = ''+Math.round(+new Date()/1000);
    await t
      .click('#menu')
      .click('li[id="menu"] li[resource="v-l:Create"]')
      .typeText('veda-control.fulltext.dropdown', 'Персона')
      .click('div.suggestion[resource="v-s:Person"]')
      .click('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]')
      .typeText('veda-control.-view.edit.search[property="v-s:lastName"]', 'Пупкин')
      .typeText('veda-control.-view.edit.search.has-error[property="v-s:firstName"]', 'Вася')
      .typeText('veda-control.-view.edit.search[property="v-s:middleName"]', timeStamp)
      .click('span[data-template="v-ui:StandardButtonsTemplate"] span[typeof="v-s:Person"] button#save')
      .click('#menu')
      .click('li[id="menu"] li[resource="v-l:Find"]')
      .click('ul#req-tabs a[about="v-fs:AttributiveBundle"]')
      .typeText('veda-control.fulltext.dropdown[rel="v-fs:typeToSearch"] textarea.form-control.fulltext[name="v_fs_attributiverequest_v_fs_typetosearch"]', 'Персона')
      .click('div.suggestion[resource="v-s:Person"]')
      .typeText('veda-control[property="v-s:middleName"] input.form-control[name="v_s_person_v_s_middlename"]', timeStamp)
      .wait(1000)
      .click('button#find')
      .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('1')
});
