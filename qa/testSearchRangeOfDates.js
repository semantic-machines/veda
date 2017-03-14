var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    person = require('./person.js');

/**
 * Проверка поиска
 *@param driver 
 *@param somethingUnique - поисковый запрос;
 *@param count - количество элементов, которое должно быть в результате;
*/

function search(driver, somethingUnique, count) {
    basic.menu(driver, 'Search');
    basic.execute(driver, 'sendKeys', '#q', "Cannot fill input field", somethingUnique);
    driver.executeScript("document.querySelector('button[id=\"search-submit\"]').scrollIntoView(true);");
    basic.isEnabled(driver, 'button[id="search-submit"]', basic.SLOW_OPERATION);
    driver.wait
    (
        function () {
            driver.findElement({css:'#search-submit'}).click();
            driver.sleep(basic.FAST_OPERATION); // Иначе слишком часто щелкает поиск
            return driver.findElement({css:'#results_count'}).getText().then(function (text) {
                return text == count;
            });
        },
        basic.EXTRA_SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Number of elements is wrong, expected: " + count);});
}

/**
 * 1.Open page -> login(as karpovrt);
 * 2.Create Person1 -> Create Person2 -> Create Person3 -> Create Person4;
 * 3.Search requests and cheking results;
 * 4.Quit;
 * 
 * 1.Открываем страницу -> Входим в систему под karpovrt;
 * 2.Создаем Персону1 -> Создаем Персону2 -> Создаем Персону3-> Создаем Персону4;
 * 3.Поисковые запросы и проверка результатов;
 * 4.Выход;
*/


basic.getDrivers().forEach(function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    person.createPerson(driver, drv, 'Range', 'Of', 'DatesO', '01.01.2014');
    person.createPerson(driver, drv, 'Range', 'Of', 'DatesO', '02.12.2015');
    person.createPerson(driver, drv, 'Range', 'Of', 'DatesO', '12.07.2016');
    var now = new Date();
    person.createPerson(driver, drv, 'Range', 'Of', 'DatesO',
        ('0' + now.getDate()).slice(-2) + '.' + ('0' + (now.getMonth() + 1)).slice(-2) + '.' + now.getFullYear());

    search(driver, "'v-s:birthday' == [2014-01-01T00:00:00, 2014-01-01T23:59:59] && 'v-s:middleName' == 'DatesO'", 1);
    search(driver, "'v-s:birthday' == [2014-01-01T00:00:00, 2015-12-02T23:59:59] && 'v-s:middleName' == 'DatesO'", 2);
    search(driver, "'v-s:birthday' == [2014-01-01T00:00:00, 2016-07-12T23:59:59] && 'v-s:middleName' == 'DatesO'", 3);
    search(driver, "'v-s:birthday' == [2014-01-01T00:00:00, " +  now.getFullYear() + '-' +
        ('0' + (now.getMonth() + 1)).slice(-2) + '-'+ ('0' + now.getDate()).slice(-2) + "T23:59:59] && 'v-s:middleName' == 'DatesO'", 4);

    driver.quit();
});
