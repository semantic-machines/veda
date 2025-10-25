import Basic from './basic';
import config from './config';
fixture `test Creating Net Interface`
  .page `${config.baseUrl}`;
const basic = new Basic();
test('testCreatingNetInterface', async (t) => {
  const username = 'karpovrt';
  const password = '123';
  const searchText = 'Сеть';
  
  console.log(`Шаг 1: Login as ${username}`);
  basic.login(username, password);
  
  console.log('Шаг 2: Setup dialog handler');
  await t.setNativeDialogHandler(() => true);
  
  console.log(`Шаг 3: Open create menu and select Net (${searchText})`);
  await t
    .click('#menu')
    .click('li[id="menu"] li[resource="v-s:Create"]')
    .typeText('veda-control.fulltext.dropdown', searchText)
    .click('.suggestion[resource="v-wf:Net"]')
    .wait(3000);
  
  console.log('Шаг 4: Test net interface buttons');
  await t
    .click('.create-task')
    .click('.state-task')
    .click('.copy-net-element')
    .click('.delete-state')
    .click('.zoom-in')
    .click('.zoom-out')
    .click('.zoom-default')
    .click('#full-width');
  
  console.log('Тест testCreatingNetInterface завершён');
});
