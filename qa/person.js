var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    assert = require('assert');

module.exports = {
    /**
     * Создать Персону с указанием уникального Имени, Фамилии, Отчества, Даты Рождения
     * @param driver
     * @param drv
     * @param lastName - Фамилия
     * @param firstName - Имя
     * @param middleName - Отчество
     * @param date - Дата рождения.
     * @param phase - Фаза текущего теста
     */
createPerson: function (driver, drv, lastName, firstName, middleName, date, phase) {
    basic.openCreateDocumentForm(driver, 'Персона', 'v-s:Person', phase);
    //driver.sleep(basic.FAST_OPERATION);
    // Документ нельзя создать или отправить пока не заполнены обязательные поля
    driver.findElement({css:'div[typeof="v-s:Person"] > button#save.action'}).isEnabled().then(function (flag) {
      assert(!flag);
    }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Save button must be inactive");});

    // Удаляем раскрытый appointment
    //driver.executeScript("document.querySelector('[rel=\"v-s:hasAppointment\"] button.button-delete').scrollIntoView(true);");
    //driver.findElement({css:'[rel="v-s:hasAppointment"] button.button-delete'}).click().thenCatch(function (e) {basic.errorHandler(e, "Cannot delete appointment")});

    // Заполняем обязательные поля
    basic.execute(driver, 'sendKeys', 'div[id="object-container"] [property="rdfs:label"] + veda-control input',
        "****** PHASE#" + phase + " : ERROR = Cannot fill 'rdfs:label' for person", lastName + " " + firstName + " " + middleName);
    basic.execute(driver, 'sendKeys', '[property="v-s:lastName"] + veda-control input', "****** PHASE#" + phase + " : ERROR = Cannot fill 'v-s:lastName' for person", lastName);
    basic.execute(driver, 'sendKeys', '[property="v-s:firstName"] + veda-control input', "****** PHASE#" + phase + " : ERROR = Cannot fill 'v-s:firstName' for person", firstName);
    basic.execute(driver, 'sendKeys', '[property="v-s:middleName"] + veda-control input', "****** PHASE#" + phase + " : ERROR = Cannot fill 'v-s:middleName' for person", middleName);

    basic.execute(driver, 'click', '[property="v-s:birthday"] + veda-control input', "****** PHASE#" + phase + " : ERROR = Cannot click on 'v-s:birthday' for person");
    basic.execute(driver, 'clear', '[property="v-s:birthday"] + veda-control input', "****** PHASE#" + phase + " : ERROR = Cannot find 'birthday'");
    basic.execute(driver, 'sendKeys', '[property="v-s:birthday"] + veda-control input', "****** PHASE#" + phase + " : ERROR = Cannot fill 'v-s:birthday' for person", date);

    basic.execute(driver, 'click', '[property="v-s:middleName"] + veda-control input', "****** PHASE#" + phase + " : ERROR = Cannot click on 'middle name control' for person");

    //basic.chooseFromDropdown(driver, 'v-s:hasAccount', 'karpovrt', 'karpovrt');

    //driver.executeScript("document.querySelector('[rel=\"v-s:hasAppointment\"] + veda-control input').scrollIntoView(true);");

    //basic.chooseFromDropdown(driver, 'v-s:hasAppointment', 'Администратор2', 'Администратор2 : Аналитик');

    driver.executeScript("$('div[typeof=\"v-s:Person\"] > button#save.action')[0].scrollIntoView(true);")
        .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Cannot scroll to save button");});

    // Документ становится возможно сохранить
    basic.isEnabled(driver, '#save', basic.FAST_OPERATION, phase);
    // Нажимаем сохранить
    basic.execute(driver, 'click', '#save', "****** PHASE#" + phase + " : ERROR = Cannot click on 'Save/Сохранить' button");
/*
    driver.findElement({css:'div[id="object-container"] > [typeof="v-s:Person"]'}).getAttribute('resource').then(function (individualId) {
      basic.openPage(driver, drv, '#/'+individualId);
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems person is not saved")});
*/
    driver.sleep(basic.SLOW_OPERATION);
    // Смотрим что в нём содержится введённый ранее текст
    driver.findElement({css:'div[property="v-s:firstName"] span[class="value-holder"]'}).getText().then(function (txt) {
      assert(txt == firstName);
    }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Seems that person is not saved properly/FN");});
    driver.findElement({css:'div[property="v-s:lastName"] span[class="value-holder"]'}).getText().then(function (txt) {
      assert(txt == lastName);
    }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Seems that person is not saved properly/LN");});
    driver.findElement({css:'div[property="v-s:middleName"] span[class="value-holder"]'}).getText().then(function (txt) {
      assert(txt == middleName);
    }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Seems that person is not saved properly/MN");});
  }
}
