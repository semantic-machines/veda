var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js');

/**
 * Проверка текущего языка
 * @param driver
 * @param language - текущий язык
 * @param value - значение, которое должно быть в 'user-info' для языка language
 */
function check(driver, language, value) {
    driver.wait
    (
        webdriver.until.elementTextContains(driver.findElement({id:'user-info'}), value),
        basic.SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Language is incorrect, expected: " + language + "get: " + value);});
}

/**
 * 1.Open Page -> Login(as karpovrt);
 * 2.Click 'Eng' button -> Check language: english, russian;
 * 3.Click 'Рус' button -> Check language: english;
 * 4.Click 'Рус' button -> Click 'Eng' button -> Check language: russian;
 * 5.Quit;
 *
 * 1.Открываем страницу -> Входим в систему под karpovrt;
 * 2.Кликаем на кнопку 'Eng' -> Проверяем, что в текущий момент активны 2 языка: русский и английский;
 * 3.Кликаем на кнопку 'Рус' -> Проверяем, что в текущий момент активен 1 язык: английский;
 * 4.Кликаем на кнопку 'Рус' -> Кликаем на кнопку 'Eng' -> Проверяем, что в данный момент активен 1 язык: русский;
 * 5.Выход;
 */

basic.getDrivers().forEach (function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);

    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    driver.findElement({css:'button[about="v-ui:EN"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Eng` button");});
    check(driver, 'Eng', "2");
    check(driver, 'Eng', "Administrator2");
    check(driver, 'Рус', "2");
    check(driver, 'Рус', "Администратор2");

    //только английский
    driver.findElement({css:'button[about="v-ui:RU"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Рус' button");});
    check(driver, 'Eng', "2");
    check(driver, 'Eng', "Administrator2");

    //только русский
    driver.findElement({css:'button[about="v-ui:RU"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Рус` button");});
    driver.findElement({css:'button[about="v-ui:EN"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Eng` button");});

    check(driver, 'Рус', "2");
    check(driver, 'Рус', "Администратор2");

    driver.quit();
});
