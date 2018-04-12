import basic from './basic'
import { Selector } from 'testcafe';

fixture `testPerson`
    .page `http://localhost:8080/`;

test('testPerson', async t => {
    await t
		.maximizeWindow()
		.typeText('#login', 'karpovrt')
		.typeText('#password', '123')
		.click('#submit')
		.expect(Selector('#user-info').innerText).eql(' Администратор2\n')
		.click('#menu')
		.click('li[id="menu"] li[resource="v-l:Create"]')
		.typeText('veda-control[rel="v-fc:targetType"] textarea', 'Персона')
		.click(Selector('veda-control[rel="v-fc:targetType"] .suggestions .suggestion').withText('Персона'))
		.wait(2000);
});
