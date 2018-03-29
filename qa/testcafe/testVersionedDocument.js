import Basic from './basic' 
import { Selector } from 'testcafe';

fixture `testVersionedDocument`
    .page `http://live.semantic-machines.com/`;

test('testVersionedDocument', async t => {
    //basic.login('karpovrt', '123');
    //basic.openCreateDocumentForm('Мероприятие');
    await t
        //.maximizeWindow()
        .typeText('#login', 'karpovrt')
        .typeText('#password', '123')
        .click('#submit')
        .click('#menu')
        .click('li[id="menu"] li[resource="v-l:Create"]')
        .typeText('veda-control.fulltext.dropdown', 'Мероприятие')
        .click('div.suggestion[about="v-s:Action"]')
        .typeText('input.form-control[lang="RU"]', 'Мероприятие')
        .typeText('input[name="v_s_action_v_s_responsible"]', 'Администратор2')
        .click('veda-control.fulltext div.tt-suggestion>p')
        .click('#save')
        .click('#edit')
        .wait(2000)
        .typeText('input.form-control[lang="RU"]', '1')
        .click('#save')
        .click('#edit')
        .wait(2000)
        .click('input.form-control[lang="RU"]')
        .pressKey('backspace')
        .typeText('input.form-control[lang="RU"]', '2')
        .click('#save')
        .click('#edit')
        .wait(2000)
        .click('input.form-control[lang="RU"]')
        .pressKey('backspace')
        .typeText('input.form-control[lang="RU"]', '3')
        .click('#save')
        .click('button.btn.btn-link.view.edit.-search.toggle-actions')
        .wait(2000)
        .click('div.col-md-8.col-xs-7.value div.ui-sortable.ui-sortable-disabled[rel="v-s:previousVersion"] a[typeof="v-s:Action v-s:Version"]')
        .expect(Selector('div.col-md-8.col-xs-7 div[property="rdfs:label"] span.value-holder').innerText).eql('Мероприятие3')
        .click('div.col-md-8.col-xs-7.value div.ui-sortable.ui-sortable-disabled[rel="v-s:previousVersion"] a[typeof="v-s:Action v-s:Version"]')
        .expect(Selector('div.col-md-8.col-xs-7 div[property="rdfs:label"] span.value-holder').innerText).eql('Мероприятие2')
        .click('div.col-md-8.col-xs-7.value div.ui-sortable.ui-sortable-disabled[rel="v-s:previousVersion"] a[typeof="v-s:Action v-s:Version"]')
        .expect(Selector('div.col-md-8.col-xs-7 div[property="rdfs:label"] span.value-holder').innerText).eql('Мероприятие1')
        .click('div.col-md-8.col-xs-7.value div.ui-sortable.ui-sortable-disabled[rel="v-s:previousVersion"] a[typeof="v-s:Action v-s:Version"]')
        .expect(Selector('div.col-md-8.col-xs-7 div[property="rdfs:label"] span.value-holder').innerText).eql('Мероприятие')
});
