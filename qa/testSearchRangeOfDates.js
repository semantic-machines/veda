var basic = require('./basic.js'),
    person = require('./person.js');

/**
 * Проверка поиска
 * @param driver
 * @param somethingUnique - поисковый запрос;
 * @param count - количество элементов, которое должно быть в результате;
 * @param phase - текущая фаза теста
*/

function search(driver, somethingUnique, count, phase) {
    basic.menu(driver, 'Search', phase);
    basic.execute(driver, 'sendKeys', '#q', "****** PHASE#" + phase + " : ERROR = Cannot fill input field", somethingUnique);
    driver.executeScript("document.querySelector('button[id=\"search-submit\"]').scrollIntoView(true);")
        .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Cannot scroll to search-submit button");});
    basic.isEnabled(driver, 'button[id="search-submit"]', basic.SLOW_OPERATION, phase);
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
    ).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Number of elements is wrong, expected: " + count);});
}

/**
 * 0.Open page -> login(as karpovrt);
 * 1.Create Person1 -> Create Person2 -> Create Person3 -> Create Person4;
 * 2.Search requests and checking results;
 * 
 * 0.Открываем страницу -> Входим в систему под karpovrt;
 * 1.Создаем Персону1 -> Создаем Персону2 -> Создаем Персону3-> Создаем Персону4;
 * 2.Поисковые запросы и проверка результатов;
*/

basic.getDrivers().forEach(function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    person.createPerson(driver, drv, 'Range', 'Of', 'DatesO', '01.01.2014', 1);
    person.createPerson(driver, drv, 'Range', 'Of', 'DatesO', '02.12.2015', 1);
    person.createPerson(driver, drv, 'Range', 'Of', 'DatesO', '12.07.2016', 1);
    var now = new Date();
    person.createPerson(driver, drv, 'Range', 'Of', 'DatesO',
        ('0' + now.getDate()).slice(-2) + '.' + ('0' + (now.getMonth() + 1)).slice(-2) + '.' + now.getFullYear(), 1);

    search(driver, "'v-s:birthday' == [2014-01-01T00:00:00, 2014-01-01T23:59:59] && 'v-s:middleName' == 'DatesO'", 1, 2);
    search(driver, "'v-s:birthday' == [2014-01-01T00:00:00, 2015-12-02T23:59:59] && 'v-s:middleName' == 'DatesO'", 2, 2);
    search(driver, "'v-s:birthday' == [2014-01-01T00:00:00, 2016-07-12T23:59:59] && 'v-s:middleName' == 'DatesO'", 3, 2);
    search(driver, "'v-s:birthday' == [2014-01-01T00:00:00, " +  now.getFullYear() + '-' +
        ('0' + (now.getMonth() + 1)).slice(-2) + '-'+ ('0' + now.getDate()).slice(-2) + "T23:59:59] && 'v-s:middleName' == 'DatesO'", 4, 2);
    driver.quit();
});
