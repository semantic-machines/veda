import Basic from './basic';
import config from './config';
import {Selector} from 'testcafe';
fixture `test Journal`
  .page `${config.baseUrl}`;
const basic = new Basic();
test('testJournal', async (t) => {
  const username = 'karpovrt';
  const password = '123';
  const searchText = 'Мероприятие';
  const responsibleText = 'Администратор2';
  const shortLabelText = 'shortLabel';
  const additionalText = '123';
  const additionalText2 = '321';
  
  console.log(`Шаг 1: Login as ${username}`);
  basic.login(username, password);
  
  const documentCreated = Selector('div#records').find('div.journal-record[typeof="v-s:DocumentCreated"]').count;
  const documentUpdated = Selector('div#records').find('div.journal-record[typeof="v-s:DocumentUpdated"]').count;
  const journal = Selector('div#records').find('div.journal-record').count;
  const timeStamp = ''+Math.round(+new Date()/1000);
  
  console.log(`Шаг 2: Create action with label ${timeStamp}`);
  await t
    .click('#menu')
    .click('li[id="menu"] li[resource="v-s:Create"]')
    .typeText('veda-control.fulltext.dropdown', searchText)
    .click('.suggestion[resource="v-s:Action"]')
    .typeText('veda-control[property="rdfs:label"] input.form-control[lang="RU"]', timeStamp)
    .typeText('veda-control[rel="v-s:responsible"] textarea[name="v_s_action_v_s_responsible"]', responsibleText)
    .wait(5000)
    .click('.suggestions .suggestion[resource="td:RomanKarpov-Analyst1"]')
    .typeText('veda-control[property="v-s:shortLabel"] textarea.form-control[lang="RU"]', shortLabelText)
    .click('button#save')
    .wait(5000);
  
  console.log('Шаг 3: Open journal and check initial records - expect 1 DocumentCreated');
  await t
    .click('button#journal')
    .wait(1000)
    .click('li[role="presentation"] span[about="v-ui:JournalTemplate"]')
    .wait(5000)
    .expect(documentCreated).eql(1)
    .expect(journal).eql(1);
  
  console.log(`Шаг 4: Edit shortLabel by adding "${additionalText}" and check journal - expect 1 DocumentUpdated`);
  await t
    .click('div.pull-left span[about="v-s:Action"]')
    .click('button#edit')
    .typeText('veda-control[property="v-s:shortLabel"] textarea.form-control[lang="RU"]', additionalText)
    .click('button#save')
    .click('button#journal')
    .wait(1000)
    .click('li[role="presentation"] span[about="v-ui:JournalTemplate"]')
    .wait(5000)
    .expect(documentUpdated).eql(1)
    .expect(journal).eql(2);
  
  console.log('Шаг 5: Save without changes and check journal - expect no new records');
  await t
    .click('div.pull-left span[about="v-s:Action"]')
    .click('button#edit')
    .click('button#save')
    .click('button#journal')
    .wait(1000)
    .click('li[role="presentation"] span[about="v-ui:JournalTemplate"]')
    .wait(5000)
    .expect(documentUpdated).eql(1)
    .expect(journal).eql(2);
  
  console.log(`Шаг 6: Edit shortLabel by adding "${additionalText2}" and check journal - expect 2 DocumentUpdated`);
  await t
    .click('div.pull-left span[about="v-s:Action"]')
    .click('button#edit')
    .typeText('veda-control[property="v-s:shortLabel"] textarea.form-control[lang="RU"]', additionalText2)
    .click('button#save')
    .click('button#journal')
    .wait(1000)
    .click('li[role="presentation"] span[about="v-ui:JournalTemplate"]')
    .wait(5000)
    .expect(documentUpdated).eql(2)
    .expect(journal).eql(3);
  
  console.log('Тест testJournal завершён');
});
