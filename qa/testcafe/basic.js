import { Selector, t } from 'testcafe';

export default class basic {
  async login(login, password) {
    await t
      .click('#login')
      .pressKey('ctrl+a delete')
      .typeText('#login', login)
      .click('#password')
      .pressKey('ctrl+a delete')
      .typeText('#password', password)
      .click('#submit-login-password')
      .wait(1000);
  }

  async logout() {
    await t
      .click('#menu')
      .click('li[id="menu"] li[resource="v-s:Exit"]');
  }

  async createPerson(last, first, middle, birthDate) {
    await t
      .click('#menu')
      .click('li[id="menu"] li[resource="v-s:Create"]')
      .click('veda-control.fulltext.dropdown')
      .pressKey('ctrl+a delete')
      .typeText('veda-control.fulltext.dropdown', 'Персона')
      .click('div.suggestion[resource="v-s:Person"]')
      .typeText('veda-control.-view.edit.search[property="v-s:lastName"]', last)
      .typeText('veda-control.-view.edit.search.has-error[property="v-s:firstName"]', first)
      .typeText('veda-control.-view.edit.search[property="v-s:middleName"]', middle)
      .click('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]')
      .pressKey('ctrl+a delete')
      .typeText('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]', birthDate)
      .click('span[data-template="v-ui:StandardButtonsTemplate"] span[typeof="v-s:Person"] button#save')
  }

  async fullTextSearch(query, eql) {
    await t
      .click('ul.nav.navbar-nav.navbar-right li[about="v-fs:FulltextSearch"]')
      .click('div.input-group input[name="v_s_userthing_*"]')
      .pressKey('ctrl+a delete')
      .typeText('div.input-group input[name="v_s_userthing_*"]', query)
      .click('span.input-group-btn button[about="v-fs:Find"]')
      .expect(Selector('small.stats-top.pull-right span.badge[property="v-fs:authorized"]').innerText).eql(eql)
  }

  async createTestUI(label, timeStamp) {
    const number = Selector('veda-control[property="v-ui:testInteger"] div select.form-control').find('option').withText('2');
    const checkbox = Selector('veda-control[rel="v-ui:testLink"] div div.checkbox').find('label').withText('Спецификация тестового объектного свойства');
    await t
      .click('#menu')
      .click('li[id="menu"] li[resource="v-s:Create"]')
      .click('veda-control.fulltext.dropdown')
      .pressKey('ctrl+a delete')
      .typeText('veda-control.fulltext.dropdown', 'Класс для тестирования интерфейса')
      .click('div.suggestion[resource="v-ui:TestUIClass"]')
      .click('div.actions.actions-fixed button[type="button"] span.glyphicon.glyphicon-chevron-left')
      .typeText('veda-control.-view.edit.search[property="rdfs:label"]', label)
      .wait(1000)
      .typeText('veda-control.-view.edit.search[property="rdfs:comment"]', timeStamp)
      .wait(1000)
      .hover('em[about="v-ui:testFile"]')
      .typeText('veda-control[property="v-ui:testString"]', 'Предопределенное значение 1')
      .wait(1000)
      .click('veda-control[data-type="select"][property="v-ui:testInteger"]')
      .click(number)
      .click(checkbox)
      .click('div.actions.actions-fixed button[type="button"] span.glyphicon.glyphicon-chevron-right')
      .click('button#save')
  }

  async createTestUiForAttributiveSearch(label, comment, testString, date) {
    const number = Selector('veda-control[property="v-ui:testInteger"] div select.form-control').find('option').withText('2');
    const checkbox = Selector('veda-control[rel="v-ui:testLink"] div div.checkbox').find('label').withText('Спецификация тестового объектного свойства');
    await t
      .click('#menu')
      .click('li[id="menu"] li[resource="v-s:Create"]')
      .click('veda-control.fulltext.dropdown')
      .pressKey('ctrl+a delete')
      .typeText('veda-control.fulltext.dropdown', 'Класс для тестирования интерфейса')
      .click('div.suggestion[resource="v-ui:TestUIClass"]')
      .click('div.actions.actions-fixed button[type="button"] span.glyphicon.glyphicon-chevron-left')
      .typeText('veda-control.-view.edit.search[property="rdfs:label"]', label)
      .wait(1000)
      .typeText('veda-control.-view.edit.search[property="rdfs:comment"]', comment)
      .wait(1000)
      .hover('em[about="v-ui:testFile"]')
      .typeText('veda-control[property="v-ui:testString"]', testString)
      .wait(1000)
      .click('veda-control[data-type="select"][property="v-ui:testInteger"]')
      .click(number)
      .click(checkbox)
      .click('veda-control#date')
      .pressKey('ctrl+a delete')
      .typeText('veda-control[property="v-ui:testDatetime"]#date', date)
      .click('div.actions.actions-fixed button[type="button"] span.glyphicon.glyphicon-chevron-right')
      .click('button#save')
  }

  async attributiveSearch(last, first, middle, birthDate, eql) {
    await t
      .click('#menu')
      .click('li[id="menu"] li[resource="v-s:Find"]')
      .click('ul#req-tabs a[about="v-fs:AttributiveBundle"]')
      //.click('veda-control.fulltext.dropdown[rel="v-fs:typeToSearch"] textarea.form-control.fulltext[name="v_fs_attributiverequest_v_fs_typetosearch"]')
      //.pressKey('ctrl+a delete')
      .typeText('veda-control.fulltext.dropdown[rel="v-fs:typeToSearch"] textarea.form-control.fulltext[name="v_fs_attributiverequest_v_fs_typetosearch"]', 'Персона')
      .click('div.suggestion[resource="v-s:Person"]')
      .click('veda-control[property="v-s:lastName"] input.form-control[name="v_s_person_v_s_lastname"]', last)
      .pressKey('ctrl+a delete')
      .typeText('veda-control[property="v-s:lastName"] input.form-control[name="v_s_person_v_s_lastname"]', last)
      .click('veda-control[property="v-s:firstName"] input.form-control[name="v_s_person_v_s_firstname"]', first)
      .pressKey('ctrl+a delete')
      .typeText('veda-control[property="v-s:firstName"] input.form-control[name="v_s_person_v_s_firstname"]', first)
      .click('veda-control[property="v-s:middleName"] input.form-control[name="v_s_person_v_s_middlename"]', middle)
      .pressKey('ctrl+a delete')
      .typeText('veda-control[property="v-s:middleName"] input.form-control[name="v_s_person_v_s_middlename"]', middle)
      .click('veda-control[property="v-s:birthday"] input.form-control[name="v_s_person_v_s_birthday"]', birthDate)
      .pressKey('ctrl+a delete')
      .typeText('veda-control[property="v-s:birthday"] input.form-control[name="v_s_person_v_s_birthday"]', birthDate)
      .click('veda-control[property="v-s:lastName"] input.form-control[name="v_s_person_v_s_lastname"]')
      .click('button#find')
      .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql(eql)
  }
  async attributiveSearchTestUi(timeStamp) {
    await t
      .click('#menu')
      .click('li[id="menu"] li[resource="v-s:Find"]')
      .click('ul#req-tabs a[about="v-fs:AttributiveBundle"]')
      .typeText('veda-control.fulltext.dropdown[rel="v-fs:typeToSearch"] textarea.form-control.fulltext[name="v_fs_attributiverequest_v_fs_typetosearch"]', 'Класс для тестирования интерфейса')
      .click('div.suggestion[resource="v-ui:TestUIClass"]')
      .typeText('veda-control.-view.edit.search[property="rdfs:comment"]', timeStamp)
      .wait(1000)
      .click('button#find')
  }

}

