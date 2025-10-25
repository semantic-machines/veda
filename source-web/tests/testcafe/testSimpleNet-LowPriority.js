import Basic from './basic';
import config from './config';
import {Selector} from 'testcafe';
fixture `test Simple Net Low Priority`
  .page `${config.baseUrl}`;
const basic = new Basic();
test('testSimpleNetLowPriority', async (t) => {
  const username = 'karpovrt';
  const password = '123';
  const searchNet = 'Сеть';
  const searchQuery = "'rdf:type'=='v-wf:StartForm' && 'rdfs:label'=='Тестовый шаблон маршрута lowPriority'";
  const statusText = 'Ожидает отправки';
  const expectedRedCount = 1;
  
  console.log(`Шаг 1: Login as ${username}`);
  basic.login(username, password);
  
  const timeStamp = ''+Math.round(+new Date()/1000);
  const red = Selector('div#workflow-canvas').find('div.state-io-condition-output[colored-to="red"]').count;
  
  console.log(`Шаг 2: Create net with label ${timeStamp}`);
  await t
    .click('#menu')
    .click('li[id="menu"] li[resource="v-s:Create"]')
    .typeText('veda-control.fulltext.dropdown', searchNet)
    .click('.suggestion[resource="v-wf:Net"]');
  
  console.log('Шаг 3: Configure net workflow');
  await t
    .click('.glyphicon-stop')
    .click('.state-io-condition-input')
    .click('div#schema')
    .click('div#object-container div#props-col div#props table#taskTemplateProperties tbody tr td span[about="rdfs:label"]')
    .typeText('div#object-container div#props-col div#props table#taskTemplateProperties tbody tr td veda-control#VClabel input.form-control', timeStamp)
    .dragToElement('.state-io-condition-input .ep', '.glyphicon-stop');
  
  console.log('Шаг 4: Save workflow');
  await t
    .click('button#workflow-save-button')
    .wait(3000);
  
  console.log('Шаг 5: Search for test start form template');
  await t
    .click('li[about="v-fs:MultiFunctionalSearch"]')
    .typeText('veda-control[property="*"] input.form-control', searchQuery)
    .click('div.input-group span.input-group-btn #custom-search-button.search-button')
    .wait(3000);
  
  console.log(`Шаг 6: Open found start form and edit it with net ${timeStamp}`);
  await t
    .click('div.results a.glyphicon.glyphicon-search')
    .click('button#edit')
    .click('veda-control[rel="v-wf:forNet"] .form-control')
    .pressKey('ctrl+a delete')
    .typeText('veda-control[rel="v-wf:forNet"] .form-control', timeStamp)
    .click(Selector('.suggestions .suggestion').withText(timeStamp));
  
  console.log(`Шаг 7: Set status to "${statusText}" and save`);
  await t
    .click('veda-control[rel="v-wf:hasStatusWorkflow"] .form-control')
    .pressKey('ctrl+a delete')
    .typeText('veda-control[rel="v-wf:hasStatusWorkflow"] .form-control', statusText)
    .click('.suggestion[resource="v-wf:ToBeSent"]')
    .click('button#save')
    .wait(3000);
  
  console.log('Шаг 8: Open process and verify red workflow element');
  await t
    .click('div[rel="v-wf:isProcess"] a')
    .expect(red).eql(expectedRedCount);
  
  console.log('Тест testSimpleNetLowPriority завершён');
});
