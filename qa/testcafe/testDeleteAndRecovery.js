import Basic from './basic'
import { Selector, t } from 'testcafe';
  fixture `test Delete And Recovery`
  .page `http://localhost:8080/`
  const basic = new Basic();
  const timeStamp = ''+Math.round(+new Date()/1000);
  const query = "'rdfs:label' == '"+ timeStamp +"' && 'v-s:deleted' == 'true'" ;
  test('testDeleteAndRecovery', async t => {
    basic.login('karpovrt', '123');
    await t
      .click('#menu')
      .click('li[id="menu"] li[resource="v-l:Create"]')
      .typeText('veda-control.fulltext.dropdown', 'Стартовая форма')
      .click('div.suggestion[resource="v-wf:StartForm"]')
      .typeText('veda-control[data-type="multilingualString"] input[type="text"]', timeStamp)
      .click('#save')
      .click('#menu')
      .click('li[id="menu"] li[resource="v-l:Find"]')
      .typeText('veda-control[property="v-fs:fulltextQuery"] input[type="text"]', timeStamp)
      .typeText('veda-control[rel="v-fs:typeToSearch"] textarea[name="v_fs_fulltextrequest_v_fs_typetosearch"]', 'Стартовая форма')
      .click('div.suggestion[resource="v-wf:StartForm"]')
      .click('div.input-group button#submit')
      .click('ol#results-list span.label-template')
      .setNativeDialogHandler(() => true)
      .click('#delete')
      .click('li[about="v-fs:FulltextSearch"]')
      .typeText('veda-control[property="*"] input.form-control', query)
      .click('div.input-group span.input-group-btn button.btn-primary.custom-find')
      .click('div.results a.glyphicon.glyphicon-search.deleted[typeof="v-wf:StartForm"]')
      .click('p#deleted-alert-msg button#deleted-alert-recover')
      .click('li[about="v-fs:FulltextSearch"]')
      .click('div.col-md-12 small.advanced-toggle.text-muted')
      .click('div[rel="rdf:type"] #rel-actions button.btn.btn-default.button-delete')
      .click('veda-control[property="*"] input.form-control')
      .pressKey('ctrl+a delete')
      .typeText('veda-control[property="*"] input.form-control', timeStamp)
      .click('div.input-group span.input-group-btn button.btn-primary.custom-find')
      .expect(Selector('small.stats-top.pull-right span[property="v-fs:estimated"]').innerText).eql('1');
  });
