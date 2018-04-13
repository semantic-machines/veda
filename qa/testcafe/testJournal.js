import Basic from './basic'
import { Selector, t } from 'testcafe';
  fixture `test Journal`
    .page `http://localhost:8080/`
  const basic = new Basic();
  test('testJournal', async t => {
    basic.login('karpovrt', '123');
    const documentCreated = Selector('div#records').find('div.journal-record[typeof="v-s:DocumentCreated"]').count;
    const documentUpdated = Selector('div#records').find('div.journal-record[typeof="v-s:DocumentUpdated"]').count;
    const journal = Selector('div#records').find('div.journal-record').count;
    const timeStamp = ''+Math.round(+new Date()/1000);
    await t
      .click('#menu')
      .click('li[id="menu"] li[resource="v-l:Create"]')
      .typeText('veda-control.fulltext.dropdown', 'Мероприятие')
      .click('div.suggestion[about="v-s:Action"]')
      .typeText('veda-control[property="rdfs:label"] input.form-control[lang="RU"]', timeStamp)
      .typeText('veda-control[rel="v-s:responsible"] textarea[name="v_s_action_v_s_responsible"]', 'Администратор2')
      .wait(2000)
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
