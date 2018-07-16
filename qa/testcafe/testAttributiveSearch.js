import Basic from './basic'
import { Selector, t } from 'testcafe';
  fixture `test Attributive Search`
    .page `http://localhost:8080/`
  const basic = new Basic();
  const first =  'xGIo5f';
  const last = 'GhiOJe';
  const middle = 'NE1UCD';
  const birth = '01.01.1990';
  test('testAttributiveSearch', async t => {
    basic.login('karpovrt', '123');
    basic.createTestUiForAttributiveSearch(last+'b', first+'cbb', middle+'Q', birth);
    basic.createTestUiForAttributiveSearch('a'+last, first+'bcc', 'T'+middle, birth);
    await t
      .click('#menu')
      .click('li[id="menu"] li[resource="v-s:Find"]')
      .click('ul#req-tabs a[about="v-fs:AttributiveBundle"]')
      .typeText('veda-control.fulltext.dropdown[rel="v-fs:typeToSearch"] textarea.form-control.fulltext[name="v_fs_attributiverequest_v_fs_typetosearch"]', 'Класс для тестирования интерфейса')
      .click('div.suggestion[resource="v-ui:TestUIClass"]')
      .typeText('veda-control[property="rdfs:label"] input.form-control[name="v_ui_testuiclass_rdfs_label"]', 'a' + last)
      .click('veda-control.-view.edit.search[property="rdfs:comment"]')
      .click('button#find')
      .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('1')

      .click('a#params-pill-at')

      .click('veda-control[property="rdfs:label"] input.form-control[name="v_ui_testuiclass_rdfs_label"]')
      .pressKey('ctrl+a delete')
      .typeText('veda-control.-view.edit.search[property="rdfs:comment"]', first.substring(0,4) + '*')
      .click('veda-control.fulltext.dropdown[rel="v-fs:typeToSearch"] textarea.form-control.fulltext[name="v_fs_attributiverequest_v_fs_typetosearch"]')
      .click('button#find')
      .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('2')

      .click('a#params-pill-at')

      .click('veda-control.-view.edit.search[property="rdfs:comment"]')
      .pressKey('ctrl+a delete')
      .typeText('veda-control.-view.edit.search[property="rdfs:comment"]', first + 'ccc')
      .click('veda-control.fulltext.dropdown[rel="v-fs:typeToSearch"] textarea.form-control.fulltext[name="v_fs_attributiverequest_v_fs_typetosearch"]')
      .click('button#find')
      .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('0')

      .click('a#params-pill-at')

      .click('veda-control.-view.edit.search[property="rdfs:comment"]')
      .pressKey('ctrl+a delete')
      .typeText('veda-control[property="v-ui:testString"]', middle)
      .click('veda-control.-view.edit.search[property="rdfs:comment"]')
      .click('button#find')
      .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('1')

      .click('a#params-pill-at')

      .click('veda-control[property="v-ui:testString"]')
      .pressKey('ctrl+a delete')
      .click('veda-control#date')
      .pressKey('ctrl+a delete')
      .typeText('veda-control[property="v-ui:testDatetime"]#date', birth)
      .click('veda-control.-view.edit.search[property="rdfs:comment"]')
      .click('button#find')
      .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('2')

      .click('a#params-pill-at')

      .click('veda-control#date')
      .pressKey('ctrl+a delete')
      .typeText('veda-control[property="v-ui:testDatetime"]#date', birth)
      .click('veda-control[property="rdfs:label"] input.form-control[name="v_ui_testuiclass_rdfs_label"]')
      .click('veda-control.-view.edit.search[property="rdfs:comment"]')
      .pressKey('ctrl+a delete')
      .typeText('veda-control.-view.edit.search[property="rdfs:comment"]', first)
      .click('button#find')
      .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('2')

      .click('a#params-pill-at')

      .click('veda-control#date')
      .pressKey('ctrl+a delete')
      .typeText('veda-control[property="v-ui:testDatetime"]#date', birth)
      .click('veda-control[property="rdfs:label"] input.form-control[name="v_ui_testuiclass_rdfs_label"]')
      .click('veda-control.-view.edit.search[property="rdfs:comment"]')
      .pressKey('ctrl+a delete')
      .typeText('veda-control.-view.edit.search[property="rdfs:comment"]', first)
      .typeText('veda-control[property="rdfs:label"] input.form-control[name="v_ui_testuiclass_rdfs_label"]', last)
      .typeText('veda-control[property="v-ui:testString"]', middle)
      .click('veda-control[property="rdfs:label"] input.form-control[name="v_ui_testuiclass_rdfs_label"]')
      .click('button#find')
      .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('1')

});
