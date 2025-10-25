import Basic from './basic';
import config from './config';
import {Selector} from 'testcafe';
fixture `test Delete And Recovery`
  .page `${config.baseUrl}`;
const basic = new Basic();
const timeStamp = '' + Math.round(+new Date()/1000);
const queryStartForm = "'rdfs:label' == '" + timeStamp + "'";
const queryDeletedStartForm = "'rdfs:label' == '" + timeStamp + "' && 'v-s:deleted' == 'true'";
const menu = Selector('#menu');
const create = Selector('li[id="menu"] li[resource="v-s:Create"]');
const searchTargetTypeInput = Selector('veda-control[rel="v-fc:targetType"] .form-control');
const startFormSuggestion = Selector('.suggestion[resource="v-wf:StartForm"]');
const labelInput = Selector('veda-control[property="rdfs:label"] .form-control');
const saveBtn = Selector('#save');
const fulltextSearch = Selector('li[about="v-fs:MultiFunctionalSearch"]');
const fulltextQueryInput = Selector('veda-control[property="*"] .form-control');
const searchBtn = Selector('.search-button');
const searchResult = Selector('.results a.glyphicon.glyphicon-search');
const deleteBtn = Selector('#delete');
const recoverBtn = Selector('#recover');
const estimatedResults = Selector('.stats-top span[property="v-fs:estimated"]');

test('testDeleteAndRecovery', async (t) => {
  const username = 'karpovrt';
  const password = '123';
  const searchText = 'Стартовая форма';
  const expectedResults = '1';
  
  console.log(`Шаг 1: Login as ${username}`);
  basic.login(username, password);
  
  console.log('Шаг 2: Setup dialog handler');
  await t.setNativeDialogHandler(() => true);
  
  console.log(`Шаг 3: Create start form with label ${timeStamp}`);
  await t
    .click(menu)
    .click(create)
    .typeText(searchTargetTypeInput, searchText)
    .click(startFormSuggestion)
    .typeText(labelInput, timeStamp)
    .click(saveBtn)
    .wait(3000);
  
  console.log(`Шаг 4: Search for created form with query: ${queryStartForm}`);
  await t
    .click(fulltextSearch)
    .typeText(fulltextQueryInput, queryStartForm)
    .click(searchBtn)
    .wait(3000);
  
  console.log('Шаг 5: Open found form and delete it');
  await t
    .click(searchResult)
    .click(deleteBtn)
    .wait(3000);
  
  console.log(`Шаг 6: Search for deleted form with query: ${queryDeletedStartForm}`);
  await t
    .click(fulltextSearch)
    .click(fulltextQueryInput)
    .pressKey('ctrl+a delete')
    .typeText(fulltextQueryInput, queryDeletedStartForm)
    .click(searchBtn)
    .wait(3000);
  
  console.log('Шаг 7: Open deleted form and recover it');
  await t
    .click(searchResult)
    .click(recoverBtn)
    .wait(3000);
  
  console.log(`Шаг 8: Search for recovered form with query: ${queryStartForm}`);
  await t
    .click(fulltextSearch)
    .click(fulltextQueryInput)
    .pressKey('ctrl+a delete')
    .typeText(fulltextQueryInput, queryStartForm)
    .click(searchBtn)
    .wait(3000);
  
  console.log(`Шаг 9: Verify recovered form is found - expect ${expectedResults} result`);
  await t
    .expect(estimatedResults.innerText).eql(expectedResults);
  
  console.log('Тест testDeleteAndRecovery завершён');
});
