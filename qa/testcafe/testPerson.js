import Basic from './basic'
import { Selector, t } from 'testcafe';
  fixture `test Person`
    .page `http://localhost:8080/`;
  const basic = new Basic();
  test('testPerson', async t => {
    basic.login('karpovrt', '123');
    const timeStamp = ''+Math.round(+new Date()/1000);
    basic.createTestUI('Вася Пупкин', timeStamp)
    await t
      .click('#menu')
      .click('li[id="menu"] li[resource="v-s:Find"]')
      .click('ul#req-tabs a[about="v-fs:AttributiveBundle"]')
      .typeText('veda-control.fulltext.dropdown[rel="v-fs:typeToSearch"] textarea.form-control.fulltext[name="v_fs_attributiverequest_v_fs_typetosearch"]', 'Класс для тестирования интерфейса')
      .click('div.suggestion[resource="v-ui:TestUIClass"]')
      .typeText('veda-control.-view.edit.search[property="rdfs:comment"]', timeStamp)
      .wait(1000)
      .click('button#find')
      .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('1')
});
