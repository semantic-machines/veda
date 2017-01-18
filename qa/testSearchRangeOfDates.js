var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    person = require('./person.js');

function search(driver, somethingUnique, count) {
    driver.findElement({id:"menu"}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button");});
    basic.isVisible(driver, 'li[id="menu"] li[resource="v-l:Search"]', basic.FAST_OPERATION);
    driver.findElement({css:'li[id="menu"] li[resource="v-l:Search"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `search` button");});
    driver.findElement({css:'#q'}).sendKeys(somethingUnique)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill input field");});
    driver.executeScript("document.querySelector('button[id=\"search-submit\"]').scrollIntoView(true);");
    basic.isEnabled(driver, 'button[id="search-submit"]', basic.SLOW_OPERATION);
    driver.wait
    (
        function () {
            driver.findElement({css:'button[id="search-submit"]'}).click();
            driver.sleep(basic.FAST_OPERATION); // Иначе слишком часто щелкает поиск
            return driver.findElement({css:'#results_count'}).getText().then(function (text) {
                return text == count;
            });
        },
        basic.EXTRA_SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Number of elements is wrong, expected: " + count);});
}


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
