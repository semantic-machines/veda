import Basic from './basic';
import config from './config';
import {Selector} from 'testcafe';
fixture `test DropDown`
  .page `${config.baseUrl}`;
const basic = new Basic();
test('testDropDown', async (t) => {
  const username = 'karpovrt';
  const password = '123';
  const searchText = 'Класс для тестирования интерфейса';
  const airTransportLabel = 'Авиатранспорт';
  const railTransportLabel = 'Железнодорожный транспорт';
  const busLabel = 'Автобус';
  
  console.log(`Шаг 1: Login as ${username}`);
  basic.login(username, password);
  
  console.log(`Шаг 2: Open create menu and select test UI class (${searchText})`);
  await t
    .click('#menu')
    .click('li[id="menu"] li[resource="v-s:Create"]')
    .click('veda-control.fulltext.dropdown')
    .typeText('veda-control.fulltext.dropdown', searchText)
    .click('.suggestion[resource="v-ui:TestUIClass"]');
  
  console.log('Шаг 3: Test select all in dropdown - expect 3 items');
  await t
    .click('veda-control[rel="v-s:hasTransportKind"] .dropdown')
    .click('veda-control[rel="v-s:hasTransportKind"] .fulltext-menu .select-all')
    .expect(Selector('veda-control[rel="v-s:hasTransportKind"] .suggestions .suggestion.selected').count).eql(3);
  
  console.log('Шаг 4: Test cancel selection - expect 0 items');
  await t
    .click('veda-control[rel="v-s:hasTransportKind"] .fulltext-menu .cancel-selection')
    .expect(Selector('veda-control[rel="v-s:hasTransportKind"] .suggestions .suggestion.selected').count).eql(0);
  
  console.log('Шаг 5: Test invert selection - expect 3 items');
  await t
    .click('veda-control[rel="v-s:hasTransportKind"] .fulltext-menu .invert-selection')
    .expect(Selector('veda-control[rel="v-s:hasTransportKind"] .suggestions .suggestion.selected').count).eql(3);
  
  console.log('Шаг 6: Unselect one item - expect 2 items');
  await t
    .click('veda-control[rel="v-s:hasTransportKind"] .fulltext-menu .suggestions .suggestion[resource="d:62fbbbc27f8e407c8f51de71e10d0501"]')
    .expect(Selector('veda-control[rel="v-s:hasTransportKind"] .suggestions .suggestion.selected').count).eql(2);
  
  console.log('Шаг 7: Test invert selection again - expect 1 item');
  await t
    .click('veda-control[rel="v-s:hasTransportKind"] .fulltext-menu .invert-selection')
    .expect(Selector('veda-control[rel="v-s:hasTransportKind"] .suggestions .suggestion.selected').count).eql(1);
  
  console.log('Шаг 8: Cancel selection and select air transport');
  await t
    .click('veda-control[rel="v-s:hasTransportKind"] .fulltext-menu .cancel-selection')
    .expect(Selector('veda-control[rel="v-s:hasTransportKind"] .suggestions .suggestion.selected').count).eql(0)
    .click('veda-control[rel="v-s:hasTransportKind"] .fulltext-menu .suggestions .suggestion[resource="d:2078749d2bcf42e0ae80c5d8287d19d1"]')
    .click('veda-control[rel="v-s:hasTransportKind"] .fulltext-menu .close-menu')
    .expect(Selector('div[rel="v-s:hasTransportKind"] span#label').innerText).eql(airTransportLabel);
  
  console.log(`Шаг 9: Select rail transport (${railTransportLabel})`);
  await t
    .click('veda-control[rel="v-s:hasTransportKind"] .dropdown')
    .click('veda-control[rel="v-s:hasTransportKind"] .fulltext-menu .cancel-selection')
    .wait(1000)
    .click('veda-control[rel="v-s:hasTransportKind"] .fulltext-menu .suggestions .suggestion[resource="d:177b4624ed3f4ce6a1c89d7a3bd4f1c1"]')
    .click('veda-control#label')
    .expect(Selector('div[rel="v-s:hasTransportKind"] span#label').innerText).eql(railTransportLabel);
  
  console.log(`Шаг 10: Double click to select bus (${busLabel})`);
  await t
    .click('veda-control[rel="v-s:hasTransportKind"] .dropdown')
    .click('veda-control[rel="v-s:hasTransportKind"] .fulltext-menu .cancel-selection')
    .wait(1000)
    .doubleClick('veda-control[rel="v-s:hasTransportKind"] .fulltext-menu .suggestions .suggestion[resource="d:62fbbbc27f8e407c8f51de71e10d0501"]')
    .expect(Selector('div[rel="v-s:hasTransportKind"] span#label').innerText).eql(busLabel);
  
  console.log(`Шаг 11: Test single select field with ${airTransportLabel}`);
  await t
    .click('veda-control[rel="v-s:hasTransportKindSingle"] .dropdown')
    .click('veda-control[rel="v-s:hasTransportKindSingle"] .fulltext-menu .suggestions .suggestion[resource="d:2078749d2bcf42e0ae80c5d8287d19d1"]')
    .expect(Selector('div[rel="v-s:hasTransportKindSingle"] span#label').innerText).eql(airTransportLabel);
  
  console.log('Шаг 12: Verify single select dropdown auto-closes');
  await t
    .expect(Selector('veda-control[rel="v-s:hasTransportKindSingle"] .fulltext-menu').visible).notOk();
  
  console.log('Тест testDropDown завершён');
});
