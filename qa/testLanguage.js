var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js');

function check(driver, language, value) {
    driver.wait
    (
        webdriver.until.elementTextContains(driver.findElement({id:'user-info'}), value),
        basic.SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Language is incorrect, expected: " + language + "get: " + value);});
}

basic.getDrivers().forEach (function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);

    basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');

    //2 языка - русский и английский
    driver.sleep(basic.SLOW_OPERATION);
    driver.findElement({css:'button[about="v-ui:EN"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Eng` button");});
    driver.sleep(basic.SLOW_OPERATION);
    check(driver, 'Eng', "Roman");
    check(driver, 'Eng', "Karpov");
    check(driver, 'Рус', "Роман");
    check(driver, 'Рус', "Карпов");

    //только английский
    driver.findElement({css:'button[about="v-ui:RU"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Рус' button");});
    driver.sleep(basic.SLOW_OPERATION);
    check(driver, 'Eng', "Roman");
    check(driver, 'Eng', "Karpov");

    //только русский
    driver.findElement({css:'button[about="v-ui:RU"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Рус` button");});
    driver.sleep(basic.SLOW_OPERATION);
    driver.findElement({css:'button[about="v-ui:EN"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Eng` button");});
    driver.sleep(basic.SLOW_OPERATION);

    check(driver, 'Рус', "Роман");
    check(driver, 'Рус', "Карпов");

    driver.quit();
});
