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
 * Нажатие на кнопку смены языка
 * @param driver
 * @param button - необходимая кнопка смены языка
 */
function click(driver, button, phase) {
    basic.execute(driver, 'click', 'button[about="v-ui:' + button + '"]', "****** PHASE#" + phase + " : ERROR = Cannot click on " + button + " button");
    driver.sleep(basic.FAST_OPERATION);
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
    //PHASE#1
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    driver.findElement({css:'#app'}).then(function(){console.log("****** PHASE#1 > LOGIN   : COMPLETE");});

    //PHASE#2
    click(driver, 'EN', 2);
    check(driver, 'Eng', "2", 2);
    check(driver, 'Eng', "Administrator2", 2);
    check(driver, 'Рус', "2", 2);
    check(driver, 'Рус', "Администратор2", 2);
    driver.findElement({css:'#app'}).then(function(){console.log("****** PHASE#2 > RUS+ENG : COMPLETE");});

    //PHASE#3: только английский
    click(driver, 'RU', 3);
    check(driver, 'Eng', "2", 3);
    check(driver, 'Eng', "Administrator2", 3);
    driver.findElement({css:'#app'}).then(function(){console.log("****** PHASE#3 > ENG     : COMPLETE");});

    //PHASE#4: только русский
    click(driver, 'RU', 4);
    click(driver, 'EN', 4);
    check(driver, 'Рус', "2", 4);
    check(driver, 'Рус', "Администратор2", 4);
    driver.findElement({css:'#app'}).then(function(){console.log("****** PHASE#4 > RUS     : COMPLETE");});

    //PHASE#5
    driver.quit();
});
