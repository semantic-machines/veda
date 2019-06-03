import Basic from './basic';
import config from './config';
import { Selector, t } from 'testcafe';
  fixture `test Delete And Recovery`
    .page `${config.baseUrl}`;
  const basic = new Basic();
  const timeStamp = ''+Math.round(+new Date()/1000);
  const searchStartForm = "'rdfs:label' == '"+ timeStamp +"'" ;
  const query = "'rdfs:label' == '"+ timeStamp +"' && 'v-s:deleted' == 'true'" ;
  test('testDeleteAndRecovery', async t => {
    basic.login('karpovrt', '123');
    await t
      .click('#menu')
      .wait(1000)
      .click('li[id="menu"] li[resource="v-s:Create"]')
      .wait(1000)
      .typeText('veda-control.fulltext.dropdown', 'Стартовая форма')
      .wait(1000)
      .click('div.suggestion[resource="v-wf:StartForm"]')
      .wait(1000)
      .typeText('veda-control[data-type="multilingualString"] input[type="text"]', timeStamp)
      .wait(1000)
      .click('#save')
      .wait(5000)
      .click('li[about="v-fs:FulltextSearch"]')
      .wait(1000)
      .typeText('veda-control[property="*"] input.form-control', searchStartForm)
      .wait(1000)
      .click('div.input-group span.input-group-btn #custom-search-button.search-button')
      .wait(1000)
      .click('div.results a.glyphicon.glyphicon-search')
      .wait(1000)
      .setNativeDialogHandler(() => true)
      .click('#delete')
      .wait(1000)
      .click('li[about="v-fs:FulltextSearch"]')
      .wait(1000)
      .click('veda-control[property="*"] input.form-control')
      .pressKey('ctrl+a delete')
      .typeText('veda-control[property="*"] input.form-control', query)
      .wait(1000)
      .click('div.input-group span.input-group-btn #custom-search-button.search-button')
      .wait(1000)
      .click('div.results a.glyphicon.glyphicon-search')
      .wait(1000)
      .click('p#deleted-alert-msg button#deleted-alert-recover')
      .wait(1000)
      .click('li[about="v-fs:FulltextSearch"]')
      .wait(1000)
      .click('.advanced-toggle')
      .wait(1000)
      .click('div[rel="rdf:type"] .rel-actions button.btn.btn-default.button-delete')
      .wait(1000)
      .click('veda-control[property="*"] input.form-control')
      .pressKey('ctrl+a delete')
      .typeText('veda-control[property="*"] input.form-control', timeStamp)
      .wait(1000)
      .click('div.input-group span.input-group-btn #custom-search-button.search-button')
      .wait(1000)
      .expect(Selector('small.stats-top.pull-right span[property="v-fs:estimated"]').innerText).eql('1');
  });
