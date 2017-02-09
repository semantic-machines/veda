var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    assert = require('assert');

module.exports = {
  /**
   * Создать Персону с указанием уникального значения в качестве отчества
   */
  createPerson: function (driver, drv, lastName, firstName, middleName, date) {
    basic.openCreateDocumentForm(driver, 'Персона', 'v-s:Person');
    //driver.sleep(basic.FAST_OPERATION);
    // Документ нельзя создать или отправить пока не заполнены обязательные поля
    driver.findElement({css:'div[typeof="v-s:Person"] > button#save.action'}).isEnabled().then(function (flag) {
      assert(!flag);
    }).thenCatch(function (e) {basic.errorHandler(e, "Save button must be inactive");});

    // Удаляем раскрытый appointment
    //driver.executeScript("document.querySelector('[rel=\"v-s:hasAppointment\"] button.button-delete').scrollIntoView(true);");
    //driver.findElement({css:'[rel="v-s:hasAppointment"] button.button-delete'}).click().thenCatch(function (e) {basic.errorHandler(e, "Cannot delete appointment")});

    // Заполняем обязательные поля
    driver.findElement({css:'div[id="object-container"] [property="rdfs:label"] + veda-control input'}).sendKeys(lastName + " " + firstName + " " + middleName)
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'rdfs:label' for person");});
    driver.findElement({css:'[property="v-s:lastName"] + veda-control input'}).sendKeys(lastName)
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'v-s:lastName' for person");});
    driver.findElement({css:'[property="v-s:firstName"] + veda-control input'}).sendKeys(firstName)
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'v-s:firstName' for person");});
    driver.findElement({css:'[property="v-s:middleName"] + veda-control input'}).sendKeys(middleName)
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'v-s:middleName' for person");});

    driver.findElement({css:'[property="v-s:birthday"] + veda-control input'}).click()
      .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'v-s:birthday' for person");});
    driver.findElement({css:'[property="v-s:birthday"] + veda-control input'}).clear();
    driver.findElement({css:'[property="v-s:birthday"] + veda-control input'}).sendKeys(date)
      .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'v-s:birthday' for person");});

    driver.findElement({css:'[property="v-s:middleName"] + veda-control input'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'middle name control' for person");});

    //basic.chooseFromDropdown(driver, 'v-s:hasAccount', 'karpovrt', 'karpovrt');

    //driver.executeScript("document.querySelector('[rel=\"v-s:hasAppointment\"] + veda-control input').scrollIntoView(true);");

    //basic.chooseFromDropdown(driver, 'v-s:hasAppointment', 'Администратор2', 'Администратор2 : Аналитик');

    driver.executeScript("$('div[typeof=\"v-s:Person\"] > button#save.action')[0].scrollIntoView(true);");

    // Документ становится возможно сохранить
    basic.isEnabled(driver, '#save', basic.FAST_OPERATION);
    // Нажимаем сохранить
    driver.findElement({css:'#save'}).click()
      .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Save/Сохранить' button");});
/*
    driver.findElement({css:'div[id="object-container"] > [typeof="v-s:Person"]'}).getAttribute('resource').then(function (individualId) {
      basic.openPage(driver, drv, '#/'+individualId);
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems person is not saved")});
*/
    driver.sleep(basic.SLOW_OPERATION);
    // Смотрим что в нём содержится введённый ранее текст
    driver.findElement({css:'div[property="v-s:firstName"] span[class="value-holder"]'}).getText().then(function (txt) {
      assert(txt == firstName);
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems that person is not saved properly/FN");});
    driver.findElement({css:'div[property="v-s:lastName"] span[class="value-holder"]'}).getText().then(function (txt) {
      assert(txt == lastName);
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems that person is not saved properly/LN");});
    driver.findElement({css:'div[property="v-s:middleName"] span[class="value-holder"]'}).getText().then(function (txt) {
      assert(txt == middleName);
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems that person is not saved properly/MN");});
  }
}
