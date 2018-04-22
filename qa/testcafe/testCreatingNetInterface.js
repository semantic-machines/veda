import Basic from './basic'
import { Selector, t } from 'testcafe';
  fixture `test Creating Net Interface`
    .page `http://localhost:8080/`;
  const basic = new Basic();
  test('testCreatingNetInterface', async t => {
    basic.login('karpovrt', '123');
    await t
      .click('#menu')
      .click('li[id="menu"] li[resource="v-l:Create"]')
      .typeText('veda-control.fulltext.dropdown', 'Сеть')
      .click('div.suggestion[resource="v-wf:Net"]')
      .setNativeDialogHandler(() => true)
      .click('.create-task')
      .click('.state-task')
      .click('.copy-net-element')
      .click('.delete-state')
      .click('.zoom-in')
      .click('.zoom-out')
      .click('.zoom-default')
      .click('#full-width');
});
