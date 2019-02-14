import Basic from './basic';
import config from './config';
import { Selector, t } from 'testcafe';
  fixture `test DropDown`
    .page `${config.baseUrl}`;
  const basic = new Basic();
  test('testDropDown', async t => {
    basic.login('karpovrt', '123');
    await t
      .click('#menu')
      .click('li[id="menu"] li[resource="v-s:Create"]')
      .click('veda-control.fulltext.dropdown')
      .typeText('veda-control.fulltext.dropdown', 'Класс для тестирования интерфейса')
      .click('div.suggestion[resource="v-ui:TestUIClass"]')
      .click('div.actions.actions-fixed button[type="button"] span.glyphicon.glyphicon-chevron-left')
      .click('veda-control[rel="v-s:hasTransportKind"] div.dropdown')
      .click('veda-control[rel="v-s:hasTransportKind"] div.fulltext-menu span.select-all')
      .expect(Selector('veda-control[rel="v-s:hasTransportKind"] div.suggestions div.suggestion.selected').count).eql(3)
      .click('veda-control[rel="v-s:hasTransportKind"] div.fulltext-menu span.cancel-selection')
      .expect(Selector('veda-control[rel="v-s:hasTransportKind"] div.suggestions div.suggestion.selected').count).eql(0)
      .click('veda-control[rel="v-s:hasTransportKind"] div.fulltext-menu span.invert-selection')
      .expect(Selector('veda-control[rel="v-s:hasTransportKind"] div.suggestions div.suggestion.selected').count).eql(3)
      .click('veda-control[rel="v-s:hasTransportKind"] div.fulltext-menu div.suggestions div[resource="d:62fbbbc27f8e407c8f51de71e10d0501"]')
      .expect(Selector('veda-control[rel="v-s:hasTransportKind"] div.suggestions div.suggestion.selected').count).eql(2)
      .click('veda-control[rel="v-s:hasTransportKind"] div.fulltext-menu span.invert-selection')
      .expect(Selector('veda-control[rel="v-s:hasTransportKind"] div.suggestions div.suggestion.selected').count).eql(1)
});
