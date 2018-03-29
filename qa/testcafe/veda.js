import Basic from './basic' 
import { Selector, t } from 'testcafe';

fixture `veda`
	.page `http://live.semantic-machines.com/`
    //.page `http://live.semantic-machines.com/`;
    
const basic = new Basic();
//работает    
test.skip('testLogin', async t => {
	basic.login('karpovrt', '123');
	basic.logout();
});
//работает
test.skip('testLanguage', async t => {
    basic.login('karpovrt', '123');
    await t
		//EN
		.click('button[about="v-ui:EN"]')
		.click('button[about="v-ui:RU"]')
		.expect(Selector('#user-info').innerText).eql('Administrator2\n')
		//RU
		.click('button[about="v-ui:RU"]')
		.click('button[about="v-ui:EN"]')
		.expect(Selector('#user-info').innerText).eql('Администратор2\n');
});
//работает
test('testDeleteAndRecovery', async t => {
	basic.login('karpovrt', '123');
	//basic.createExample('DARExample');
        const timeStamp = ''+Math.round(+new Date()/1000);
        const query = "'rdfs:label' == '"+ timeStamp +"' && 'v-s:deleted' == 'true'" ;
	await t
                .click('#menu')
                .click('li[id="menu"] li[resource="v-l:Create"]')
                .typeText('veda-control.fulltext.dropdown', 'Стартовая форма')
                .click('div.suggestion[about="v-wf:StartForm"]')
		.typeText('veda-control[data-type="multilingualString"] input[type="text"]', timeStamp)
		.click('#save')
		.click('#menu')
		.click('li[id="menu"] li[resource="v-l:Find"]')
		.typeText('veda-control[property="v-fs:fulltextQuery"] input[type="text"]', timeStamp)
		.typeText('veda-control[rel="v-fs:typeToSearch"] textarea[name="v_fs_fulltextrequest_v_fs_typetosearch"]', 'Стартовая форма')
		.click('div.suggestion[about="v-wf:StartForm"]')
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
                .click('veda-control[property="*"] input.form-control')
                .pressKey('ctrl+a delete')
		.typeText('veda-control[property="*"] input.form-control', timeStamp)
		.click('div.input-group span.input-group-btn button.btn-primary.custom-find')
		.expect(Selector('small.stats-top.pull-right span[property="v-fs:estimated"]').innerText).eql('1');	
});
//работает
test('testRights', async t => {
	basic.login('karpovrt', '123');
	//basic.createExample('rightsxExample');
	const user = 'Стивен Эдвин Кинг';
        const user1 = 'Говард Филлипс Лавкрафт';
        await t
            //.typeText('#login', 'karpovrt')
            //.typeText('#password', '123')
            //.click('#submit')
            //.expect(Selector('#user-info').innerText).eql('Администратор2\n')
            .click('#menu')
            .click('li[id="menu"] li[resource="v-l:Create"]')
            .typeText('veda-control.fulltext.dropdown', 'Персона')
            .click('div.suggestion[about="v-s:Person"]')
            .click('veda-control.-view.edit.search[property="v-s:birthday"]')
            .typeText('veda-control.-view.edit.search[property="v-s:lastName"]', 'Кинг')
            .typeText('veda-control.-view.edit.search.has-error[property="v-s:firstName"]', 'Стивен')
            .typeText('veda-control.-view.edit.search[property="v-s:middleName"]', 'Эдвин')
            //.typeText('veda-control.-view.edit.search[property="rdfs:label"]', user)
            .wait(1000)
            .click('span[data-template="v-ui:StandardButtonsTemplate"] span[typeof="v-s:Person"] button#save')
            .click('#menu')
            .click('li[id="menu"] li[resource="v-l:Exit"]')
            .click('#login')
            .pressKey('ctrl+a delete')
            .typeText('#login', 'bychinat')
            .click('#submit')
            .expect(Selector('#user-info').innerText).eql('Администратор4\n')
            //для поиска через меню
            //.click('#menu')
            //.click('li[id="menu"] li[resource="v-l:Find"]')
            //.typeText('input.form-control[name="v_fs_fulltextrequest_v_fs_fulltextquery"]', user)
            //.typeText('veda-control.fulltext.dropdown', 'Персона')
            //.click('div.suggestion[about="v-s:Person"]')
            //.click('span.input-group-btn button#submit')
            //для поиска через v-fs:FulltextSearch
            .click('ul.nav.navbar-nav.navbar-right li[about="v-fs:FulltextSearch"]')
            .typeText('div.input-group input[name="v_s_userthing_*"]', user)
            .click('span.input-group-btn button[about="v-fs:Find"]')
            .expect(Selector('div.not-found.alert.alert-warning strong[about="v-fs:Empty"]').innerText).eql('Пусто!')
            .click('#menu')
            .click('li[id="menu"] li[resource="v-l:Create"]')
            .typeText('veda-control.fulltext.dropdown', 'Персона')
            .click('div.suggestion[about="v-s:Person"]')
            .click('veda-control.-view.edit.search[property="v-s:birthday"]')
            .typeText('veda-control.-view.edit.search[property="v-s:lastName"]', 'Лавкрафт')
            .typeText('veda-control.-view.edit.search.has-error[property="v-s:firstName"]', 'Говард')
            .typeText('veda-control.-view.edit.search[property="v-s:middleName"]', 'Филлипс')
            //.typeText('veda-control.-view.edit.search[property="rdfs:label"]', user1)
            .wait(1000)
            .click('span[data-template="v-ui:StandardButtonsTemplate"] span[typeof="v-s:Person"] button#save')
            .click('#menu')
            .click('li[id="menu"] li[resource="v-l:Exit"]')
            .click('#login')
            .pressKey('ctrl+a delete')
            .typeText('#login', 'karpovrt')
            .click('#submit')
            .expect(Selector('#user-info').innerText).eql('Администратор2\n')
            .click('ul.nav.navbar-nav.navbar-right li[about="v-fs:FulltextSearch"]')
            .typeText('div.input-group input[name="v_s_userthing_*"]', user1)
            .click('span.input-group-btn button[about="v-fs:Find"]')
            .click('tbody.result-container a.glyphicon.glyphicon-search')
            .expect(Selector('div.col-md-9 h3 span[property="v-s:lastName"]').innerText).eql('Лавкрафт')
            .expect(Selector('div.col-md-9 h3 span[property="v-s:firstName"]').innerText).eql('Говард')
            .expect(Selector('div.col-md-9 h3 span[property="v-s:middleName"]').innerText).eql('Филлипс')
		
});
//не работает, нужно переписать
test.skip('testComment', async t => {
	basic.login('karpovrt', '123');
	await t
		.click('#user-info')
		.click('#add-comment')
		.typeText('div[typeof="v-s:Comment"] textarea[class="form-control"]', '12345')  //type comment
		.click('div[typeof="v-s:Comment"] button[id="save"]')
		.click('#reply')
		.typeText('div[typeof="v-s:Comment"] textarea[class="form-control"]', '12345')  //type reply-comment
		.click('div[typeof="v-s:Comment"] button[id="save"]')
		//check buttons
		.expect(Selector('#reply').count).eql(2)
		.expect(Selector('#edit-comment').count).eql(2)
		.expect(Selector('a[id="edit-comment"][style="display: none;"]').count).eql(1)
		.expect(Selector('a[id="delete"][style="display: none;"]').count).eql(1)
		.expect(Selector('a[id="delete"][about="v-s:Delete"]').count).eql(2)
		.setNativeDialogHandler(() => true)
		.click(Selector('a[id="delete"][about="v-s:Delete"]').nth(1))         //delete reply-comment
		.wait(3000)
		//check buttons
		.expect(Selector('#reply').count).eql(1)
		.expect(Selector('#edit-comment').count).eql(1)
		.expect(Selector('a[id="delete"][about="v-s:Delete"]').count).eql(1)
		.click('a[id="delete"][about="v-s:Delete"]');                 //delete comment
		
});
//работает
test('testVersionedDocument', async t => {
        basic.login('karpovrt', '123');
        //basic.openCreateDocumentForm('Мероприятие');
        await t
                //.typeText('#login', 'karpovrt')
                //.typeText('#password', '123')
                //.click('#submit')
                .click('#menu')
                .click('li[id="menu"] li[resource="v-l:Create"]')
                .typeText('veda-control.fulltext.dropdown', 'Мероприятие')
                .click('div.suggestion[about="v-s:Action"]')
                .typeText('input.form-control[lang="RU"]', 'Мероприятие')
                .typeText('veda-control[rel="v-s:responsible"] textarea[name="v_s_action_v_s_responsible"]', 'Администратор2')
                .click('div.suggestion[about="td:RomanKarpov-Analyst1"]')
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
                //.click('button.btn.btn-link.view.edit.-search.toggle-actions')
                .wait(2000)
                .hover('div[rel="v-s:responsible"]')
                .click('div.col-md-8.col-xs-7.value div.ui-sortable.ui-sortable-disabled[rel="v-s:previousVersion"] a[typeof="v-s:Action v-s:Version"]')
                .expect(Selector('div.col-md-8.col-xs-7 div[property="rdfs:label"] span.value-holder').innerText).eql('Мероприятие3')
                .hover('div[rel="v-s:responsible"]')
                .click('div.col-md-8.col-xs-7.value div.ui-sortable.ui-sortable-disabled[rel="v-s:previousVersion"] a[typeof="v-s:Action v-s:Version"]')
                .expect(Selector('div.col-md-8.col-xs-7 div[property="rdfs:label"] span.value-holder').innerText).eql('Мероприятие2')
                .hover('div[rel="v-s:responsible"]')
                .click('div.col-md-8.col-xs-7.value div.ui-sortable.ui-sortable-disabled[rel="v-s:previousVersion"] a[typeof="v-s:Action v-s:Version"]')
                .expect(Selector('div.col-md-8.col-xs-7 div[property="rdfs:label"] span.value-holder').innerText).eql('Мероприятие1')
                .hover('div[rel="v-s:responsible"]')
                .click('div.col-md-8.col-xs-7.value div.ui-sortable.ui-sortable-disabled[rel="v-s:previousVersion"] a[typeof="v-s:Action v-s:Version"]')
                .expect(Selector('div.col-md-8.col-xs-7 div[property="rdfs:label"] span.value-holder').innerText).eql('Мероприятие')
});
//нужно разобраться, как передать куки
test.skip('testAttachment', async t => {
	basic.login('karpovrt', '123');
});
//работает
test('testSearchRangeOfDates', async t => {
        basic.login('karpovrt', '123');
        const birthDate =  '01.01.2014';
        const birthDate1 = '02.12.2015';
        const birthDate2 = '12.07.2016';
        const query = '\'v-s:birthday\' == [2014-01-01T00:00:00, 2014-01-01T23:59:59] && \'v-s:middleName\' == \'DatesO\'';
        const query1 = '\'v-s:birthday\' == [2014-01-01T00:00:00, 2015-12-02T23:59:59] && \'v-s:middleName\' == \'DatesO\'';
        const query2 = '\'v-s:birthday\' == [2014-01-01T00:00:00, 2016-07-12T23:59:59] && \'v-s:middleName\' == \'DatesO\'';
        const query3 = '\'v-s:birthday\' == [2014-01-01T00:00:00, 2034-07-12T23:59:59] && \'v-s:middleName\' == \'DatesO\'';
        await t
                //.typeText('#login', 'karpovrt')
                //.typeText('#password', '123')
                //.click('#submit')
                //.expect(Selector('#user-info').innerText).eql('Администратор2\n')
                .click('#menu')
                .click('li[id="menu"] li[resource="v-l:Create"]')
                .typeText('veda-control.fulltext.dropdown', 'Персона')
                .click('div.suggestion[about="v-s:Person"]')

                .typeText('veda-control.-view.edit.search[property="v-s:lastName"]', 'Range')
                .typeText('veda-control.-view.edit.search.has-error[property="v-s:firstName"]', 'Of')
                .typeText('veda-control.-view.edit.search[property="v-s:middleName"]', 'DatesO')
                .click('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]')
                .pressKey('ctrl+a delete')
                .typeText('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]', birthDate)
                .click('veda-control.-view.edit.search[property="v-s:lastName"]')
                .click('span[data-template="v-ui:StandardButtonsTemplate"] span[typeof="v-s:Person"] button#save')

                .click('#menu')
                .click('li[id="menu"] li[resource="v-l:Create"]')      
                .typeText('veda-control.-view.edit.search[property="v-s:lastName"]', 'Range')
                .typeText('veda-control.-view.edit.search.has-error[property="v-s:firstName"]', 'Of')
                .typeText('veda-control.-view.edit.search[property="v-s:middleName"]', 'DatesO')
                .click('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]')
                .pressKey('ctrl+a delete')
                .typeText('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]', birthDate1)
                .click('veda-control.-view.edit.search[property="v-s:lastName"]')
                .click('span[data-template="v-ui:StandardButtonsTemplate"] span[typeof="v-s:Person"] button#save')
                
                .click('#menu')
                .click('li[id="menu"] li[resource="v-l:Create"]')  
                .typeText('veda-control.-view.edit.search[property="v-s:lastName"]', 'Range')
                .typeText('veda-control.-view.edit.search.has-error[property="v-s:firstName"]', 'Of')
                .typeText('veda-control.-view.edit.search[property="v-s:middleName"]', 'DatesO')
                .click('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]')
                .pressKey('ctrl+a delete')
                .typeText('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]', birthDate2)
                .click('veda-control.-view.edit.search[property="v-s:lastName"]')
                .click('span[data-template="v-ui:StandardButtonsTemplate"] span[typeof="v-s:Person"] button#save')
                
                .click('#menu')
                .click('li[id="menu"] li[resource="v-l:Create"]')
                .typeText('veda-control.-view.edit.search[property="v-s:lastName"]', 'Range')
                .typeText('veda-control.-view.edit.search.has-error[property="v-s:firstName"]', 'Of')
                .typeText('veda-control.-view.edit.search[property="v-s:middleName"]', 'DatesO')
                .click('div.input-group.date input.form-control[name="v_s_person_v_s_birthday"]')
                .click('veda-control.-view.edit.search[property="v-s:lastName"]')
                .click('span[data-template="v-ui:StandardButtonsTemplate"] span[typeof="v-s:Person"] button#save')
                
                .click('ul.nav.navbar-nav.navbar-right li[about="v-fs:FulltextSearch"]')
                .typeText('div.input-group input[name="v_s_userthing_*"]', query)
                .click('span.input-group-btn button[about="v-fs:Find"]')
                .expect(Selector('small.stats-top.pull-right span.badge[property="v-fs:authorized"]').innerText).eql('1')
                
                .click('div.input-group input[name="v_s_userthing_*"]')
                .pressKey('ctrl+a delete')
                .typeText('div.input-group input[name="v_s_userthing_*"]', query1)
                .click('span.input-group-btn button[about="v-fs:Find"]')
                .expect(Selector('small.stats-top.pull-right span.badge[property="v-fs:authorized"]').innerText).eql('2')
                
                .click('div.input-group input[name="v_s_userthing_*"]')
                .pressKey('ctrl+a delete')
                .typeText('div.input-group input[name="v_s_userthing_*"]', query2)
                .click('span.input-group-btn button[about="v-fs:Find"]')
                .expect(Selector('small.stats-top.pull-right span.badge[property="v-fs:authorized"]').innerText).eql('3')
                
                .click('div.input-group input[name="v_s_userthing_*"]')
                .pressKey('ctrl+a delete')
                .typeText('div.input-group input[name="v_s_userthing_*"]', query3)
                .click('span.input-group-btn button[about="v-fs:Find"]')
                .expect(Selector('small.stats-top.pull-right span.badge[property="v-fs:authorized"]').innerText).eql('4')
});
//работает
test('testAttributiveSearchPerson', async t => {
	basic.login('karpovrt', '123');
        const first =  'xGIo5f';
        const last = 'GhiOJe';
        const middle = 'NE1UCD';
        const birth = '01.01.1990';
        await t
                //.typeText('#login', 'karpovrt')
                //.typeText('#password', '123')
                //.click('#submit')
                //.expect(Selector('#user-info').innerText).eql('Администратор2\n')
                
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
                .click('veda-control[property="v-s:lastName"] input.form-control[name="v_s_person_v_s_lastname"]')
                .pressKey('ctrl+a delete')
                
                .typeText('veda-control[property="v-s:firstName"] input.form-control[name="v_s_person_v_s_firstname"]', first.substring(0,4) + '*')
                .click('veda-control[property="v-s:lastName"] input.form-control[name="v_s_person_v_s_lastname"]')
                .click('button#find')
                .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('2')
                
                .click('a#params-pill-at')
                .click('veda-control[property="v-s:firstName"] input.form-control[name="v_s_person_v_s_firstname"]')
                .pressKey('ctrl+a delete')
                
                .typeText('veda-control[property="v-s:firstName"] input.form-control[name="v_s_person_v_s_firstname"]', first + 'ccc')
                .click('veda-control[property="v-s:lastName"] input.form-control[name="v_s_person_v_s_lastname"]')
                .click('button#find')
                .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('0')
                
                .click('a#params-pill-at')
                .click('veda-control[property="v-s:firstName"] input.form-control[name="v_s_person_v_s_firstname"]')
                .pressKey('ctrl+a delete')
                
                .typeText('veda-control[property="v-s:middleName"] input.form-control[name="v_s_person_v_s_middlename"]', middle)
                .click('veda-control[property="v-s:lastName"] input.form-control[name="v_s_person_v_s_lastname"]')
                .click('button#find')
                .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('1')
                
                .click('a#params-pill-at')
                .click('veda-control[property="v-s:middleName"] input.form-control[name="v_s_person_v_s_middlename"]')
                .pressKey('ctrl+a delete')
                
                .click('veda-control[property="v-s:birthday"] input.form-control[name="v_s_person_v_s_birthday"]')
                .pressKey('ctrl+a delete')
                .typeText('veda-control[property="v-s:birthday"] input.form-control[name="v_s_person_v_s_birthday"]', birth)
                .click('veda-control[property="v-s:lastName"] input.form-control[name="v_s_person_v_s_lastname"]')
                .click('button#find')
                .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('2')
                
                .click('a#params-pill-at')
                .click('veda-control[property="v-s:birthday"] input.form-control[name="v_s_person_v_s_birthday"]')
                .pressKey('ctrl+a delete')
                
                .click('veda-control[property="v-s:birthday"] input.form-control[name="v_s_person_v_s_birthday"]')
                .pressKey('ctrl+a delete')
                .typeText('veda-control[property="v-s:birthday"] input.form-control[name="v_s_person_v_s_birthday"]', birth)
                .typeText('veda-control[property="v-s:firstName"] input.form-control[name="v_s_person_v_s_firstname"]', first)
                .click('veda-control[property="v-s:lastName"] input.form-control[name="v_s_person_v_s_lastname"]')
                .click('button#find')
                .expect(Selector('a#results-pill-at span#results-count.badge').innerText).eql('2')
                
                .click('a#params-pill-at')
                .click('veda-control[property="v-s:birthday"] input.form-control[name="v_s_person_v_s_birthday"]')
                .pressKey('ctrl+a delete')
                .click('veda-control[property="v-s:firstName"] input.form-control[name="v_s_person_v_s_firstname"]')
                .pressKey('ctrl+a delete')
                
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
//работает
test('testJournal', async t => {
        basic.login('karpovrt', '123');
        //basic.openCreateDocumentForm('Мероприятие');
        const timeStamp = ''+Math.round(+new Date()/1000);
        const documentCreated = Selector('div#records').find('div.journal-record[typeof="v-s:DocumentCreated"]').count;
        const documentUpdated = Selector('div#records').find('div.journal-record[typeof="v-s:DocumentUpdated"]').count;
        const journal = Selector('div#records').find('div.journal-record').count;
        await t
                //Создаем Мероприятие(заполняем label, responsible, shortLabel) -> Сохраняем;
                //.typeText('#login', 'karpovrt')
                //.typeText('#password', '123')
                //.click('#submit')
                .click('#menu')
                .click('li[id="menu"] li[resource="v-l:Create"]')
                .typeText('veda-control.fulltext.dropdown', 'Мероприятие')
                .click('div.suggestion[about="v-s:Action"]')
                .typeText('veda-control[property="rdfs:label"] input.form-control[lang="RU"]', timeStamp)
                .typeText('veda-control[rel="v-s:responsible"] textarea[name="v_s_action_v_s_responsible"]', 'Администратор2')
                .click('div.suggestions div.suggestion[about="td:RomanKarpov-Analyst1"]')
                .typeText('veda-control[property="v-s:shortLabel"] textarea.form-control[lang="RU"]', 'shortLabel')
                .click('button#save')
                .click('button#journal')
                //Проверяем количество записей в журнале
                .click('li[role="presentation"] span[about="v-ui:JournalTemplate"]')
                .expect(documentCreated).eql(1)
                .expect(journal).eql(1)
                //Изменяем shortLabel -> Проверяем количество записей в журнале
                .click('div.pull-left span[about="v-s:Action"]')
                .click('button#edit')
                .typeText('veda-control[property="v-s:shortLabel"] textarea.form-control[lang="RU"]', '123')
                .click('button#save')
                .click('button#journal')
                .click('li[role="presentation"] span[about="v-ui:JournalTemplate"]')
                .wait(1000)
                .expect(documentUpdated).eql(1)
                .expect(journal).eql(2)
                //не изменяем shortlabel -> Проверяем количество записей в журнале
                .click('div.pull-left span[about="v-s:Action"]')
                .click('button#edit')
                .click('button#save')
                .click('button#journal')
                .click('li[role="presentation"] span[about="v-ui:JournalTemplate"]')
                .expect(documentUpdated).eql(1)
                .expect(journal).eql(2)
                //Изменяем shortLabel -> Проверяем количество записей в журнале
                .click('div.pull-left span[about="v-s:Action"]')
                .click('button#edit')
                .typeText('veda-control[property="v-s:shortLabel"] textarea.form-control[lang="RU"]', '321')
                .click('button#save')
                .click('button#journal')
                .click('li[role="presentation"] span[about="v-ui:JournalTemplate"]')
                .wait(1000)
                .expect(documentUpdated).eql(2)
                .expect(journal).eql(3)
});
//работает
test('testCreatingNetInterface', async t => {
	basic.login('karpovrt', '123');
	basic.openCreateDocumentForm('Сеть');
	//to do: check flow, check each click
	await t
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
//работает
test('testSimpleNet', async t => {
	basic.login('karpovrt', '123');
	//basic.openCreateDocumentForm('Сеть');
	const timeStamp = ''+Math.round(+new Date()/1000);
        const red = Selector('div#workflow-canvas').find('div.state-io-condition-output[colored-to="red"]').count;
        await t
                //.typeText('#login', 'karpovrt')
                //.typeText('#password', '123')
                //.click('#submit')
                .click('#menu')
                .click('li[id="menu"] li[resource="v-l:Create"]')
                .typeText('veda-control.fulltext.dropdown', 'Сеть')
                .click('div.suggestion[about="v-wf:Net"]')
                .click('div#schema')
                .click('div.col-md-10.col-sm-9 div#holder div.tab-content div#object-container div#props-col div#props table#taskTemplateProperties tbody tr td span[about="rdfs:label"]')
                .typeText('div.col-md-10.col-sm-9 div#holder div.tab-content div#object-container div#props-col div#props table#taskTemplateProperties tbody tr td veda-control#VClabel input.form-control', timeStamp)
                .dragToElement('.state-io-condition-input .ep', '.glyphicon-stop')
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
                .expect(red).eql(1)
});
//работает
test('testSimpleNet2', async t => {
	basic.login('karpovrt', '123');
	//basic.openCreateDocumentForm('Сеть');
	//input-task-output
        const timeStamp = ''+Math.round(+new Date()/1000);
        const red = Selector('div#workflow-canvas').find('div.state-io-condition-output[colored-to="red"]').count;
        const red1 = Selector('div#workflow-canvas').find('div.state-io-condition-input[colored-to="red"]').count;
        const green = Selector('div#workflow-canvas').find('div.state-task[colored-to="green"]').count;
        await t
                //.typeText('#login', 'karpovrt')
                //.typeText('#password', '123')
                //.click('#submit')
                .click('#menu')
                .click('li[id="menu"] li[resource="v-l:Create"]')
                .typeText('veda-control.fulltext.dropdown', 'Сеть')
                .click('div.suggestion[about="v-wf:Net"]')
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
                .click('div.suggestion[about="v-wf:StartForm"]')
                .typeText('veda-control[rel="v-wf:forNet"]', timeStamp)
                .click('div.suggestions div.suggestion[typeof="v-wf:Net"]')
                .typeText('veda-control[rel="v-wf:hasStatusWorkflow"]', 'Ожидает отправки')
                .click('div.suggestion[about="v-wf:ToBeSent"]')
                .click('button#save')
                .wait(2000)
                .click('button.btn.btn-link.view.edit.-search.toggle-actions')
                .click('div[rel="v-wf:isProcess"] span#label')
                .expect(red).eql(1)
                .expect(red1).eql(1)
                .expect(green).eql(1)

});
//работает
test('testSimpleNet3', async t => {
	basic.login('karpovrt', '123');
	//basic.openCreateDocumentForm('Сеть');
	//input-executor-output
        const timeStamp = ''+Math.round(+new Date()/1000);
        //const red = Selector('div#workflow-canvas').find('div.state-io-condition-output[colored-to="red"]').count;
        const red1 = Selector('div#workflow-canvas').find('div.state-io-condition-input[colored-to="red"]').count;
        const green = Selector('div#workflow-canvas').find('div.state-task[colored-to="red"]').count;
        await t
                //.typeText('#login', 'karpovrt')
                //.typeText('#password', '123')
                //.click('#submit')
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
                .expect(red1).eql(1)
                .expect(green).eql(1)
});





