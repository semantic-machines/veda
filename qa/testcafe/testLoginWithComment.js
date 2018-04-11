import { Selector } from 'testcafe';
  fixture `test comment`
    .page `http://localhost:8080/`

  test('testComment', async t => {
//    basic.login('karpovrt', '123');
    await t
      .typeText('#login', 'Karpovrt')
      .typeText('#password', '123')
      .click("#submit")
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


