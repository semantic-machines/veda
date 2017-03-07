var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    person = require('./person.js'),
    timeStamp = ''+Math.round(+new Date()/1000),
    assert = require('assert');

/**
 * Проверка количества всех элементов, созданыых элементов, обновленных элементов в журнале
 *@param driver
 *@param totalCount - необходимое количество всех элементов в журнале
 *@param createCount - необходимое количество созданных элементов в журнале
 *@param updateCount - необходимое количество обновленных элементов в журнале
*/

function assertCounts(driver, totalCount, createCount, updateCount) {
  //    Go to journal
  driver.wait
  (
    function () {
      driver.sleep(basic.FAST_OPERATION);
      return webdriver.until.elementLocated({css:'div.journal-record'});
    },
    basic.SLOW_OPERATION
  ).thenCatch(function (e) {basic.errorHandler(e, "Cannot find action, after save operation");});
  driver.executeScript("document.querySelector('#journal').scrollIntoView(true);");
  driver.sleep(basic.SLOW_OPERATION).then(function() {
    basic.execute(driver, 'click', '#journal', "Cannot click on `View Journal` button");
  }).then(function() {
    driver.sleep(basic.FAST_OPERATION);
    driver.navigate().refresh();
    driver.sleep(basic.SLOW_OPERATION);
    driver.findElements({css:'div.journal-record'}).then(function (result) {
      assert.equal(totalCount, result.length);
    }).thenCatch(function (e) {basic.errorHandler(e, "Invalid `total` journal elements count");});
    driver.findElements({css:'div.journal-record[typeof="v-s:DocumentCreated"]'}).then(function (result) {
      assert.equal(createCount, result.length);
    }).thenCatch(function (e) {basic.errorHandler(e, "Invalid `create` journal elements count");});
    driver.findElements({css:'div.journal-record[typeof="v-s:DocumentUpdated"]'}).then(function (result) {
      assert.equal(updateCount, result.length);
    }).thenCatch(function (e) {basic.errorHandler(e, "Invalid `update` journal elements count");});
    //    Return to document
    basic.execute(driver, 'click', '[rel="v-s:onDocument"] [typeof="v-s:Action"] a', "Cannot click to return on main document");
  });
}

/**
 * Обновление текущего значения shortLabel
 *@param driver 
 *@param key - значение, на которое необходимо поменять текущее shortLabel;
*/

function update(driver, key) {
  basic.isEnabled(driver, '#edit', basic.FAST_OPERATION);
  driver.executeScript("document.querySelector('#edit').scrollIntoView(true);");
  basic.execute(driver, 'click' , '#edit', "Cannot click on `Edit` button");
  if (key != '') {
    driver.executeScript("document.querySelector('strong[about=\"v-s:shortLabel\"]').scrollIntoView(true);");
    basic.execute(driver, 'clear', 'veda-control[property="v-s:shortLabel"] div[class="input-group"] textarea[class="form-control"]', "Cannot find 'shortLabel'");
    basic.execute(driver, 'click', 'veda-control[property="v-s:shortLabel"] div[class="input-group"] textarea[class="form-control"]',
        "Cannot fill 'v-s:shortLabel' field");
  }
  driver.executeScript("document.querySelector('#save').scrollIntoView(true);");
  basic.execute(driver, 'click', 'button[id="save"]', "Cannot click on `Save` button");
}

/**
 * 1.Open page -> Login(as karpovrt);
 * 2.Create Action(fill label, responsible, shortLabel) -> Save;
 * 3.Check journal -> Update shortLabel ->
 * -> Check journal -> No update shortLabel ->
 * -> Check journal -> Update shortLabel ->
 * -> Check journal;
 * 4.Quit;
 *
 * 1.Открываем страницу -> Входим в систему под karpovrt;
 * 2.Создаем Мероприятие(заполняем label, responsible, shortLabel) -> Сохраняем;
 * 3.Проверяем количество записей в журнале -> Изменяем shortLabel ->
 * -> Проверяем количество записей в журнале -> Не изменяем shortlabel ->
 * -> Проверяем количество записей в журнале -> Изменяем shortLabel ->
 * -> Проверяем количество записей в журнале;
 * 4.Выход;
*/


basic.getDrivers().forEach (function (drv) {
  var driver = basic.getDriver(drv);
  basic.openPage(driver, drv);
  basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

  basic.openCreateDocumentForm(driver, 'Мероприятие', "v-s:Action");
  driver.executeScript("document.querySelector('div[property=\"rdfs:label\"]').scrollIntoView(true);");
  basic.execute(driver, 'sendKeys', 'veda-control[property="rdfs:label"] div[class="input-group"] input[type="text"]',
      "Cannot fill 'rdfs:label' field", timeStamp);
  driver.executeScript("document.querySelector('strong[about=\"v-s:responsible\"]').scrollIntoView(true);");
  basic.chooseFromDropdown(driver, 'v-s:responsible', "Администратор2", "Администратор2 : Аналитик");
  driver.executeScript("document.querySelector('strong[about=\"v-s:shortLabel\"]').scrollIntoView(true);");
  basic.execute(driver, 'sendKeys', 'veda-control[property="v-s:shortLabel"] div[class="input-group"] textarea[class="form-control"]',
      "Cannot fill 'v-s:shortLabel' field", 'timeStamp + 1');
  driver.executeScript("document.querySelector('#save').scrollIntoView(true);");
  basic.execute(driver, 'click', 'button[id="save"]', "Cannot click on `Save` button");

  assertCounts(driver, 1, 1, 0);
  update(driver, timeStamp + 2);
  assertCounts(driver, 2, 1, 1);
  update(driver, '');
  assertCounts(driver, 2, 1, 1);
  update(driver, timeStamp + 3)
  assertCounts(driver, 3, 1, 2);

  driver.quit();
});
