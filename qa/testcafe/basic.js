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
      .click('#submit');
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

  async openFulltextSearchDocumentForm(name) {
    await t
      .click('#menu')
      .click('li[id="menu"] li[resource="v-s:Find"]')
      .typeText('div[typeof="v-fs:FulltextRequest"] input.fulltext.tt-input', name)

  }
}

