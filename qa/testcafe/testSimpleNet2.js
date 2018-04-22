import Basic from './basic'
import { Selector, t } from 'testcafe';
  fixture `test Simple Net2`
    .page `http://localhost:8080/`;
  const basic = new Basic();
  test('testSimpleNet2', async t => {
    basic.login('karpovrt', '123');
    const timeStamp = ''+Math.round(+new Date()/1000);
    const red = Selector('div#workflow-canvas').find('div.state-io-condition-output[colored-to="red"]').count;
    const red1 = Selector('div#workflow-canvas').find('div.state-io-condition-input[colored-to="red"]').count;
    const green = Selector('div#workflow-canvas').find('div.state-task[colored-to="green"]').count;
    await t
      .click('#menu')
      .click('li[id="menu"] li[resource="v-l:Create"]')
      .typeText('veda-control.fulltext.dropdown', 'Сеть')
      .click('div.suggestion[resource="v-wf:Net"]')
      .click('div#schema')
      .click('div.col-md-10.col-sm-9 div#holder div.tab-content div#object-container div#props-col div#props table#taskTemplateProperties tbody tr td span[about="rdfs:label"]')
      .typeText('div.col-md-10.col-sm-9 div#holder div.tab-content div#object-container div#props-col div#props table#taskTemplateProperties tbody tr td veda-control#VClabel input.form-control', timeStamp)
      .click('button.create-task')
      .dragToElement('.state-io-condition-input .ep', '.state-task')
      .dragToElement('.state-task .ep', '.glyphicon-stop')
      .click('button#workflow-save-button')
      .click('#menu')
      .click('li[id="menu"] li[resource="v-l:Create"]')
      .click('veda-control.fulltext.dropdown')
      .pressKey('ctrl+a delete')
      .typeText('veda-control.fulltext.dropdown', 'Стартовая форма')
      .click('div.suggestion[resource="v-wf:StartForm"]')
      .typeText('veda-control[rel="v-wf:forNet"]', timeStamp)
      .click(Selector('div.suggestions div.suggestion').withText(timeStamp))
      .typeText('veda-control[rel="v-wf:hasStatusWorkflow"]', 'Ожидает отправки')
      .wait(2000)
      .click('div.suggestion[resource="v-wf:ToBeSent"]')
      .click('button#save')
      .wait(2000)
      .click('button.btn.btn-link.view.edit.-search.toggle-actions')
      .click('div[rel="v-wf:isProcess"] span#label')
      .expect(red).eql(1)
      .expect(red1).eql(1)
      .expect(green).eql(1)
});
