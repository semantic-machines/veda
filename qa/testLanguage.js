var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js');

/**
 * Проверка текущего языка
 * @param driver
 * @param language - текущий язык
 * @param value - значение, которое должно быть в 'user-info' для языка language
 */
function check(driver, language, value, phase) {
    driver.wait
    (
        webdriver.until.elementTextContains(driver.findElement({id:'user-info'}), value),
        basic.SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Language is incorrect, expected: " + language + "get: " + value);});
}
/**
 * Нажатие на кнопку языка
 * @param driver
 * @param button - кнопка
 */
function click(driver, button, phase) {
    basic.execute(driver, 'click', 'button[about="v-ui:' + button + '"]', "****** PHASE#" + phase + " : ERROR = Cannot click on " + button + " button");
    driver.sleep(basic.FAST_OPERATION);
}
/**
 * 0.Open Page -> Login(as karpovrt);
 * 1.Click 'Eng' button -> Check language: english+russian;
 * 2.Click 'Рус' button -> Check language: english;
 * 3.Click 'Рус' button -> Click 'Eng' button -> Check language: russian;
 *
 * 0.Открываем страницу -> Входим в систему под karpovrt;
 * 1.Кликаем на кнопку 'Eng' -> Проверяем, что в текущий момент активны 2 языка: русский и английский;
 * 2.Кликаем на кнопку 'Рус' -> Проверяем, что в текущий момент активен 1 язык: английский;
 * 3.Кликаем на кнопку 'Рус' -> Кликаем на кнопку 'Eng' -> Проверяем, что в данный момент активен 1 язык: русский;
 */

basic.getDrivers().forEach (function (drv) {
    //PHASE#0: Login
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    //PHASE#1: RU + EN
    click(driver, 'EN', 1);
    check(driver, 'Eng', "2", 1);
    check(driver, 'Eng', "Administrator2", 1);
    check(driver, 'Рус', "2", 1);
    check(driver, 'Рус', "Администратор2", 1);

    //PHASE#2: EN
    click(driver, 'RU', 2);
    check(driver, 'Eng', "2", 2);
    check(driver, 'Eng', "Administrator2", 2);

    //PHASE#3: RU
    click(driver, 'RU', 3);
    click(driver, 'EN', 3);
    check(driver, 'Рус', "2", 3);
    check(driver, 'Рус', "Администратор2", 3);
    driver.quit();
});
