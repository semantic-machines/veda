import { Selector } from 'testcafe';

fixture `testPerson`
    .page `http://localhost:8080/`;

test('testPerson', async t => {
    await t
//    .maximizeWindow()
    .typeText('#login', 'karpovrt')
    .typeText('#password', '123')
    .click('#submit')
    .expect(Selector('#user-info').innerText).eql('Администратор2\n')
    .click('#menu')
    .click('li[id="menu"] li[resource="v-l:Create"]')
    .typeText('input.fulltext.tt-input', 'Персона')
    .click(Selector('veda-control.fulltext div.tt-suggestion>p').withText('Персона'))
    .wait(2000);
});
