import Basic from './basic';
import config from './config';
import {Selector} from 'testcafe';
fixture `test Simple Net`
  .page `${config.baseUrl}`;
const basic = new Basic();
test('testSimpleNet', async (t) => {
  const username = 'karpovrt';
  const password = '123';
  const searchNet = 'Сеть';
  const searchStartForm = 'Стартовая форма';
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
  
  console.log('Шаг 4: Save workflow and return to main page');
  await t
    .click('button#workflow-save-button')
    .wait(3000)
    .click('.navbar-brand');
  
  console.log(`Шаг 5: Create start form for net (${searchStartForm})`);
  await t
    .click('#menu')
    .click('li[id="menu"] li[resource="v-s:Create"]')
    .click('veda-control.fulltext.dropdown')
    .pressKey('ctrl+a delete')
    .typeText('veda-control.fulltext.dropdown', searchStartForm)
    .click('.suggestion[resource="v-wf:StartForm"]');
  
  console.log(`Шаг 6: Link start form to net ${timeStamp}`);
  await t
    .typeText('veda-control[rel="v-wf:forNet"]', timeStamp)
    .click(Selector('.suggestions .suggestion').withText(timeStamp));
  
  console.log(`Шаг 7: Set status to "${statusText}" and save`);
  await t
    .typeText('veda-control[rel="v-wf:hasStatusWorkflow"]', statusText)
    .click('.suggestion[resource="v-wf:ToBeSent"]')
    .click('button#save')
    .wait(10000);
  
  console.log('Шаг 8: Open process and verify red workflow element');
  await t
    .click('button#toggle-actions')
    .click('div[rel="v-wf:isProcess"] span#label')
    .wait(10000)
    .expect(red).eql(expectedRedCount);
  
  console.log('Тест testSimpleNet завершён');
});
