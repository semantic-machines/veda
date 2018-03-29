import Basic from './basic' 
import { Selector } from 'testcafe';

fixture `testSimpleNet`
    .page `http://live.semantic-machines.com/`;

test('testSimpleNet', async t => {
    //basic.login('karpovrt', '123');
    //basic.openCreateDocumentForm('Мероприятие');
    const timeStamp = ''+Math.round(+new Date()/1000);
    //const red = Selector('div#workflow-canvas').find('div.state-io-condition-output[colored-to="red"]').count;
    const red1 = Selector('div#workflow-canvas').find('div.state-io-condition-input[colored-to="red"]').count;
    const green = Selector('div#workflow-canvas').find('div.state-task[colored-to="red"]').count;
    await t
        .typeText('#login', 'karpovrt')
        .typeText('#password', '123')
        .click('#submit')
        .click('#menu')
        .click('li[id="menu"] li[resource="v-l:Create"]')
        .typeText('veda-control.fulltext.dropdown', 'Сеть')
        .click('div.suggestion[about="v-wf:Net"]')
        .click('div#schema')
        .click('div.col-md-10.col-sm-9 div#holder div.tab-content div#object-container div#props-col div#props table#taskTemplateProperties tbody tr td span[about="rdfs:label"]')
        .typeText('div.col-md-10.col-sm-9 div#holder div.tab-content div#object-container div#props-col div#props table#taskTemplateProperties tbody tr td veda-control#VClabel input.form-control', timeStamp)
        
        .click('button.create-task')
        .click('div.state-task')
        .click('div.col-md-10.col-sm-9 div#holder div.tab-content div#object-container div#props-col div#props table#taskTemplateProperties tbody tr td span[about="v-wf:executor"]')
        .typeText('div.col-md-10.col-sm-9 div#holder div.tab-content div#object-container div#props-col div#props table#taskTemplateProperties tbody tr td veda-control[rel="v-wf:executor"] textarea.form-control', 'Администратор4')
        .click('div.suggestion[typeof="v-s:Appointment"]')
        
        .dragToElement('.state-io-condition-input .ep', '.state-task')
        .dragToElement('.state-task .ep', '.glyphicon-stop')
        
        .click('button#workflow-save-button')
        
        .click('#menu')
        .click('li[id="menu"] li[resource="v-l:Create"]')
        .click('veda-control.fulltext.dropdown')
        .pressKey('ctrl+a delete')
        .typeText('veda-control.fulltext.dropdown', 'Стартовая форма')
        .click('div.suggestion[about="v-wf:StartForm"]')
        .typeText('veda-control[rel="v-wf:forNet"]', timeStamp)
        .click('div.suggestions div.suggestion[typeof="v-wf:Net"]')
        .typeText('veda-control[rel="v-wf:hasStatusWorkflow"]', 'Ожидает отправки')
        .click('div.suggestion[about="v-wf:ToBeSent"]')
        .click('button#save')
        .wait(2000)
        .click('button.btn.btn-link.view.edit.-search.toggle-actions')
        .click('div[rel="v-wf:isProcess"] span#label')
        //.expect(red).eql(1)
        .expect(red1).eql(1)
        .expect(green).eql(1)
        
        .wait(6000)
        
});
