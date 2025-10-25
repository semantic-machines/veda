import Basic from './basic';
import config from './config';
import {Selector} from 'testcafe';
fixture `test div controls`
  .page `${config.baseUrl}`;
const basic = new Basic();
const checkBox = Selector('veda-control[data-type="checkbox"] div.checkbox').find('label').withText('Спецификация тестового объектного свойства');
const radioButton = Selector('veda-control[data-type="radio"] div.radio').find('label').withText('Спецификация тестового календарного свойства');
test('testDivControls', async (t) => {
  const username = 'karpovrt';
  const password = '123';
  const testLabel = 'Тест контролов';
  const testString = 'Тестовое текстовое свойство';
  const testInteger = '111777';
  const testIntegerFormatted = '111 777';
  const testDecimal = '1113,14159265';
  const testDecimalFormatted = '1 113,14159265';
  const testDate = '29051990';
  const testDateFormatted = '29.05.1990';
  const testDateTime = '280519891232';
  const testDateTimeFormatted = '28.05.1989 12:32';
  const searchText = 'Класс для тестирования интерфейса';
  
  console.log(`Шаг 1: Login as ${username}`);
  basic.login(username, password);
  
  console.log(`Шаг 2: Open create menu and select test UI class (${searchText})`);
  await t
    .click('#menu')
    .click('li[id="menu"] li[resource="v-s:Create"]')
    .click('veda-control.fulltext.dropdown')
    .pressKey('ctrl+a delete')
    .typeText('veda-control.fulltext.dropdown', searchText)
    .click('.suggestion[resource="v-ui:TestUIClass"]');
  
  console.log(`Шаг 3: Type label "${testLabel}" and verify`);
  await t
    .typeText('veda-control.-view.edit.search[property="rdfs:label"]', testLabel)
    .wait(1000)
    .expect(Selector('div.container.sheet h3[property="rdfs:label"] span.value-holder').innerText).eql(testLabel);
  
  console.log(`Шаг 4: Type testString "${testString}" and verify`);
  await t
    .typeText('veda-control#testString', testString)
    .wait(1000)
    .expect(Selector('div.container.sheet div[property="v-ui:testString"] span.value-holder').innerText).eql(testString);
  
  console.log(`Шаг 5: Type testInteger ${testInteger} and verify formatting ${testIntegerFormatted}`);
  await t
    .click('veda-control[property="v-ui:testInteger"][data-type="integer"]')
    .typeText('veda-control[property="v-ui:testInteger"][data-type="integer"]', testInteger)
    .wait(1000)
    .expect(Selector('div.container.sheet div[property="v-ui:testInteger"] span.value-holder').innerText).eql(testIntegerFormatted);
  
  console.log(`Шаг 6: Type testDecimal ${testDecimal} and verify formatting ${testDecimalFormatted}`);
  await t
    .click('veda-control[property="v-ui:testDecimal"][data-type="decimal"]')
    .pressKey('ctrl+a delete')
    .wait(200)
    .typeText('veda-control[property="v-ui:testDecimal"][data-type="decimal"]', testDecimal, {paste: true})
    .click('veda-control[property="v-ui:testInteger"]')
    .wait(1000)
    .expect(Selector('div.container.sheet div[property="v-ui:testDecimal"] span.value-holder').innerText).eql(testDecimalFormatted);
  
  console.log(`Шаг 7: Type date ${testDate} and verify ${testDateFormatted}`);
  await t
    .click('veda-control#date')
    .pressKey('ctrl+a delete')
    .typeText('veda-control#date', testDate)
    .click('veda-control[property="v-ui:testDecimal"]')
    .wait(1000)
    .expect(Selector('div.container.sheet div[property="v-ui:testDatetime"] span.value-holder').innerText).eql(testDateFormatted);
  
  console.log(`Шаг 8: Type datetime ${testDateTime} and verify ${testDateTimeFormatted}`);
  await t
    .click('veda-control[data-type="dateTime"]')
    .pressKey('ctrl+a delete')
    .typeText('veda-control[data-type="dateTime"]', testDateTime)
    .click('veda-control[property="v-ui:testDecimal"]')
    .wait(1000)
    .expect(Selector('div.container.sheet div[property="v-ui:testDatetime"] span.value-holder').innerText).eql(testDateTimeFormatted);
  
  console.log('Шаг 9: Click checkbox and verify');
  await t
    .click(checkBox)
    .wait(1000)
    .expect(Selector('div.container.sheet div[rel="v-ui:testLink"] span#label').innerText).eql('Спецификация тестового объектного свойства');
  
  console.log('Шаг 10: Click radio button and verify');
  await t
    .click(radioButton)
    .wait(1000)
    .expect(Selector('div.container.sheet div[rel="v-ui:testLink"] span#label').innerText).eql('Спецификация тестового календарного свойства');
  
  console.log('Тест testDivControls завершён');
});
