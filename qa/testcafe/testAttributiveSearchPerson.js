import { Selector } from 'testcafe';

fixture `testAttributiveSearchPerson`
    .page `http://localhost:8080/`;
test('testAttributiveSearchPerson', async t => {
    const first =  'xGIo5f';
    const last = 'GhiOJe';
    const middle = 'NE1UCD';
    const birth = '01.01.1990';
    await t
        .typeText('#login', 'karpovrt')
        .typeText('#password', '123')
        .click('#submit')
        .expect(Selector('#user-info').innerText).eql('Администратор2\n')
        
        .click('#menu')
        .click('li[id="menu"] li[resource="v-l:Create"]')
        .typeText('veda-control.fulltext.dropdown', 'Персона')
        .click('div.suggestion[about="v-s:Person"]')
        .typeText('veda-control.-view.edit.search[property="v-s:lastName"]', 'a'+last)
        .typeText('veda-control.-view.edit.search.has-error[property="v-s:firstName"]', first+'bcc')
        .typeText('veda-control.-view.edit.search[property="v-s:middleName"]', 'T'+middle)
        .click('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]')
        .pressKey('ctrl+a delete')
        .typeText('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]', birth)
        .click('veda-control.-view.edit.search[property="v-s:lastName"]')
        .click('span[data-template="v-ui:StandardButtonsTemplate"] span[typeof="v-s:Person"] button#save')
        
        .click('#menu')
        .click('li[id="menu"] li[resource="v-l:Create"]')
        .click('veda-control.fulltext.dropdown')
        .pressKey('ctrl+a delete')
        .typeText('veda-control.fulltext.dropdown', 'Персона')
        .click('div.suggestion[about="v-s:Person"]')
        .typeText('veda-control.-view.edit.search[property="v-s:lastName"]', last+'b')
        .typeText('veda-control.-view.edit.search.has-error[property="v-s:firstName"]', first+'cbb')
        .typeText('veda-control.-view.edit.search[property="v-s:middleName"]', middle+'Q')
        .click('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]')
        .pressKey('ctrl+a delete')
        .typeText('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]', birth)
        .click('veda-control.-view.edit.search[property="v-s:lastName"]')
        .click('span[data-template="v-ui:StandardButtonsTemplate"] span[typeof="v-s:Person"] button#save')
        
        .click('#menu')
        .click('li[id="menu"] li[resource="v-l:Find"]')
        .click('ul#req-tabs a[about="v-fs:AttributiveBundle"]')
        .typeText('veda-control.fulltext.dropdown[rel="v-fs:typeToSearch"] textarea.form-control.fulltext[name="v_fs_attributiverequest_v_fs_typetosearch"]', 'Персона')
        .click('div.suggestion[about="v-s:Person"]')
        .typeText('veda-control[property="v-s:lastName"] input.form-control[name="v_s_person_v_s_lastname"]', 'a' + last)
        .click('veda-control[property="v-s:firstName"] input.form-control[name="v_s_person_v_s_firstname"]')
        .click('button#find')
        .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('1')
        
        .click('a#params-pill-at')
        .click('div[property="v-s:lastName"] button.btn.btn-default span.glyphicon.glyphicon-remove')
        
        .typeText('veda-control[property="v-s:firstName"] input.form-control[name="v_s_person_v_s_firstname"]', first.substring(0,4) + '*')
        .click('veda-control[property="v-s:lastName"] input.form-control[name="v_s_person_v_s_lastname"]')
        .click('button#find')
        .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('2')
        
        .click('a#params-pill-at')
        .click('div[property="v-s:firstName"] button.btn.btn-default span.glyphicon.glyphicon-remove')
        
        .typeText('veda-control[property="v-s:firstName"] input.form-control[name="v_s_person_v_s_firstname"]', first + 'ccc')
        .click('veda-control[property="v-s:lastName"] input.form-control[name="v_s_person_v_s_lastname"]')
        .click('button#find')
        .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('0')
        
        .click('a#params-pill-at')
        .click('div[property="v-s:firstName"] button.btn.btn-default span.glyphicon.glyphicon-remove')
        
        .typeText('veda-control[property="v-s:middleName"] input.form-control[name="v_s_person_v_s_middlename"]', middle)
        .click('veda-control[property="v-s:lastName"] input.form-control[name="v_s_person_v_s_lastname"]')
        .click('button#find')
        .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('1')
        
        .click('a#params-pill-at')
        .click('div[property="v-s:middleName"] button.btn.btn-default span.glyphicon.glyphicon-remove')
        
        .click('veda-control[property="v-s:birthday"] input.form-control[name="v_s_person_v_s_birthday"]')
        .pressKey('ctrl+a delete')
        .typeText('veda-control[property="v-s:birthday"] input.form-control[name="v_s_person_v_s_birthday"]', birth)
        .click('veda-control[property="v-s:lastName"] input.form-control[name="v_s_person_v_s_lastname"]')
        .click('button#find')
        .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('2')
        
        .click('a#params-pill-at')
        .click('div[property="v-s:birthday"] button.btn.btn-default span.glyphicon.glyphicon-remove')
        
        .click('veda-control[property="v-s:birthday"] input.form-control[name="v_s_person_v_s_birthday"]')
        .pressKey('ctrl+a delete')
        .typeText('veda-control[property="v-s:birthday"] input.form-control[name="v_s_person_v_s_birthday"]', birth)
        .typeText('veda-control[property="v-s:firstName"] input.form-control[name="v_s_person_v_s_firstname"]', first)
        .click('veda-control[property="v-s:lastName"] input.form-control[name="v_s_person_v_s_lastname"]')
        .click('button#find')
        .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('2')
        
        .click('a#params-pill-at')
        .click('div[property="v-s:birthday"] button.btn.btn-default span.glyphicon.glyphicon-remove')
        .click('div[property="v-s:firstName"] button.btn.btn-default span.glyphicon.glyphicon-remove')
        
        .click('veda-control[property="v-s:birthday"] input.form-control[name="v_s_person_v_s_birthday"]')
        .pressKey('ctrl+a delete')
        .typeText('veda-control[property="v-s:birthday"] input.form-control[name="v_s_person_v_s_birthday"]', birth)
        .typeText('veda-control[property="v-s:firstName"] input.form-control[name="v_s_person_v_s_firstname"]', first)
        .typeText('veda-control[property="v-s:lastName"] input.form-control[name="v_s_person_v_s_lastname"]', last)
        .typeText('veda-control[property="v-s:middleName"] input.form-control[name="v_s_person_v_s_middlename"]', middle)
        .click('veda-control[property="v-s:lastName"] input.form-control[name="v_s_person_v_s_lastname"]')
        .click('button#find')
        .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('1')

});
