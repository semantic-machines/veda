import Basic from './basic' 
import { Selector, t } from 'testcafe';

fixture `veda`
	.page `http://localhost:8080/`
    //.page `http://live.semantic-machines.com/`;
    
const basic = new Basic();
    
test.skip('testLogin', async t => {
	basic.login('karpovrt', '123');
	basic.logout();
});

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

test.skip('testDeleteAndRecovery', async t => {
	basic.login('karpovrt', '123');
	basic.createExample('DARExample');
	
	
	
});

test.skip('testRights', async t => {
	basic.login('karpovrt', '123');
	basic.createExample('rightsxExample');
	
		
});

test.skip('testComment', async t => {
	basic.login('karpovrt', '123');
	await t
		.click('#user-info')
		.click('#add-comment')
		.typeText('div[typeof="v-s:Comment"] textarea[class="form-control"]', '12345') 	//type comment
		.click('div[typeof="v-s:Comment"] button[id="save"]')
		.click('#reply')
		.typeText('div[typeof="v-s:Comment"] textarea[class="form-control"]', '12345') 	//type reply-comment
		.click('div[typeof="v-s:Comment"] button[id="save"]')
		
		.expect(Selector('a[id="reply"][style="display: none;"]').count).eql(0)			//
		.expect(Selector('a[id="edit-comment"][style="display: none;"]').count).eql(1)	//check buttons
		.expect(Selector('a[id="delete"][style="display: none;"]').count).eql(1)		//
		
		.setNativeDialogHandler(() => true)
		.click(Selector('a[id="delete"][about="v-s:Delete"]').nth(1))					//delete reply-comment
		
		.expect(Selector('a[id="reply"][style="display: none;"]').count).eql(0)			//
		.expect(Selector('a[id="edit-comment"][style="display: none;"]').count).eql(0)	//check buttons
		.expect(Selector('a[id="delete"][style="display: none;"]').count).eql(0)		//
		
		.click('a[id="delete"][about="v-s:Delete"]');									//delete comment
		
});

test.skip('testVersionedDocument', async t => {
	basic.login('karpovrt', '123');
});

test.skip('testAttachment', async t => {
	basic.login('karpovrt', '123');
});

test.skip('testSearchRangeOfDates', async t => {
	basic.login('karpovrt', '123');
});

test.skip('testAttributiveSearchPerson', async t => {
	basic.login('karpovrt', '123');
});

test.skip('testJournal', async t => {
	basic.login('karpovrt', '123');
});

test.skip('testCreatingNetInterface', async t => {
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

test.skip('testSimpleNet', async t => {
	basic.login('karpovrt', '123');
	basic.openCreateDocumentForm('Сеть');
	//input-output
});

test.skip('testSimpleNet2', async t => {
	basic.login('karpovrt', '123');
	basic.openCreateDocumentForm('Сеть');
	//input-task-output
});

test.skip('testSimpleNet3', async t => {
	basic.login('karpovrt', '123');
	basic.openCreateDocumentForm('Сеть');
	//input-executor-output
});





