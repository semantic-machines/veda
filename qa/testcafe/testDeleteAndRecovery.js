import { Selector } from 'testcafe';

const query = "'rdfs:label' == '12345' && 'v-s:deleted' == 'true'" ;

fixture `test Delete And Recovery`
    .page `http://live.semantic-machines.com/`;

test('test Delete And Recovery', async t => {
    await t
    //.maximizeWindow()
    .typeText('#login', 'karpovrt')
    .typeText('#password', '123')
    .click('#submit')
    .expect(Selector('#user-info').innerText).eql('Администратор2\n')
    .click('#menu')
    .click('li[id="menu"] li[resource="v-l:Create"]')
    .typeText('input.fulltext.tt-input', 'Стартовая форма')
    .click('veda-control.fulltext div.tt-suggestion>p')
    .typeText('veda-control[data-type="multilingualString"] input[type="text"]', '12345')
    .click('#save')
    .click('#menu')
    .click('li[id="menu"] li[resource="v-l:Find"]')
    .typeText('veda-control[property="v-fs:fulltextQuery"] input[type="text"]', '12345')
    .typeText('input.fulltext.tt-input', 'Стартовая форма')
    .click('veda-control.fulltext div.tt-suggestion>p')
    .click('div.input-group button#submit')
    .click('ol#results-list span.label-template')
    .setNativeDialogHandler(() => true)
    .click('#delete')
    .click('li[about="v-fs:FulltextSearch"]')
    .typeText('veda-control[property="*"] input.form-control', query)
    .click('div.input-group span.input-group-btn button.btn-primary.custom-find')
    .click('table.table.table-bordered.table-condensed a.glyphicon.glyphicon-search.deleted')
    .click('p#deleted-alert-msg button#deleted-alert-recover')
    .click('li[about="v-fs:FulltextSearch"]')
    .click('div.col-md-12 small.advanced-toggle.text-muted')
    .click('div[rel="rdf:type"] #rel-actions button.btn.btn-default.button-delete')
    .typeText('veda-control[property="*"] input.form-control', '12345')
    .click('div.input-group span.input-group-btn button.btn-primary.custom-find')
    .expect(Selector('small.stats-top.pull-right span[property="v-fs:estimated"]').innerText).eql('1')
});
