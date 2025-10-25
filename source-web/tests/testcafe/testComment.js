import Basic from './basic';
import config from './config';
import {Selector} from 'testcafe';
fixture `test Comment`
  .page `${config.baseUrl}`;
const basic = new Basic();
const timeStamp = ''+Math.round(+new Date()/1000);
const pageForNavigateFromConfig = `${config.baseUrl}`+'#/v-ui:TestUIRegistry';
test('testComment', async (t) => {
  const username = 'karpovrt';
  const password = '123';
  const testLabel = 'Тест комментария';
  const commentText = '12345';
  
  console.log(`Шаг 1: Login as ${username}`);
  basic.login(username, password);
  
  console.log(`Шаг 2: Create test UI (${testLabel}, ${timeStamp})`);
  basic.createTestUI(testLabel, timeStamp);
  
  console.log('Шаг 3: Setup dialog handler and verify user');
  await t
    .setNativeDialogHandler(() => true)
    .expect(Selector('#user-info').innerText).contains('Администратор2 .');
  
  console.log(`Шаг 4: Navigate to test registry and search for timeStamp ${timeStamp}`);
  await t
    .navigateTo( pageForNavigateFromConfig )
    .wait(1000)
    .typeText('veda-control#comment', timeStamp)
    .wait(1000)
    .click('button#search-button')
    .wait(1000);
  
  console.log('Шаг 5: Open found document');
  await t
    .click('div.search-result.noSwipe tbody.result-container a.glyphicon.glyphicon-search')
    .wait(1000);
  
  console.log(`Шаг 6: Add comment with text "${commentText}"`);
  await t
    .click('#add-comment')
    .typeText('div[typeof="v-s:Comment"] textarea[class="form-control"]', commentText)
    .wait(1000)
    .click('div[typeof="v-s:Comment"] button[id="save"]')
    .wait(3000);
  
  console.log(`Шаг 7: Add reply comment with text "${commentText}"`);
  await t
    .click('#reply')
    .typeText('div[typeof="v-s:Comment"] textarea[class="form-control"]', commentText)
    .wait(1000)
    .click('div[typeof="v-s:Comment"] button[id="save"]')
    .wait(3000);
  
  console.log('Шаг 8: Check buttons count after adding reply');
  await t
    .expect(Selector('#reply').count).eql(2)
    .expect(Selector('#edit-comment').count).eql(2)
    .expect(Selector('a[id="edit-comment"][style="display: none;"]').count).eql(1)
    .expect(Selector('a[id="delete"][style="display: none;"]').count).eql(1)
    .expect(Selector('a[id="delete"][about="v-s:Delete"]').count).eql(2);
  
  console.log('Шаг 9: Delete reply comment');
  await t
    .click(Selector('a[id="delete"][about="v-s:Delete"]').nth(1))
    .wait(3000);
  
  console.log('Шаг 10: Check buttons count after deleting reply');
  await t
    .expect(Selector('#reply').count).eql(1)
    .expect(Selector('#edit-comment').count).eql(1)
    .expect(Selector('a[id="delete"][about="v-s:Delete"]').count).eql(1);
  
  console.log('Шаг 11: Delete main comment');
  await t
    .click('a[id="delete"][about="v-s:Delete"]')
    .wait(3000);
  
  console.log('Шаг 12: Verify all comments are deleted');
  await t
    .expect(Selector('#reply').count).eql(0)
    .expect(Selector('#edit-comment').count).eql(0)
    .expect(Selector('a[id="delete"][about="v-s:Delete"]').count).eql(0);
  
  console.log('Тест testComment завершён');
});
