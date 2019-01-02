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
      .wait(2000)
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
      .wait(2000)
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
      .wait(2000)
  }

}

