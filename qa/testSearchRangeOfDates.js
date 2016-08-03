var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    assert = require('assert'),
    person = require('./person.js');

function search(driver, somethingUnique, count) {
    driver.findElement({id:"menu"}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button")});
    driver.wait
    (
        webdriver.until.elementIsVisible(driver.findElement({css:'li[id="menu"] li[resource="v-l:Search"]'})),
        basic.FAST_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Seems there is no `search` button inside menu")});
    driver.findElement({css:'li[id="menu"] li[resource="v-l:Search"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `search` button")});
    driver.findElement({css:'#q'}).sendKeys(somethingUnique)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill input field")});

    driver.executeScript("document.querySelector('button[id=\"search-submit\"]').scrollIntoView(true);");
    driver.wait
    (
        webdriver.until.elementIsEnabled(driver.findElement({css:'button[id="search-submit"]'})),
        basic.SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Cannot find search-submit button")});
    driver.sleep(basic.FAST_OPERATION);


    driver.sleep(basic.EXTRA_SLOW_OPERATION);
    driver.wait
    (
        function () {
            driver.findElement({css:'button[id="search-submit"]'}).click();
            driver.sleep(basic.FAST_OPERATION); // Иначе слишком часто щелкает поиск
            return driver.findElement({css:'#results_count'}).getText().then(function (text) {
                return text >= count
            });
        },
        basic.EXTRA_SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Number of elements is wrong, expected: " + count)});
}

function create(driver, date) {
    basic.openCreateDocumentForm(driver, 'Стартовая форма', 'v-wf:StartForm');
    driver.executeScript("document.querySelector('strong[about=\"v-s:created\"]').scrollIntoView(true);");
    driver.findElement({css:'veda-control[type="dateTime"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click label field")});
    driver.findElement({css:'veda-control[type="dateTime"] input[type="text"]'}).clear()
    driver.findElement({css:'veda-control[type="dateTime"] input[type="text"]'}).sendKeys(date)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill label field")});
    driver.executeScript("document.querySelector('button[id=\"save\"]').scrollIntoView(true);");
    driver.wait
    (
        webdriver.until.elementIsEnabled(driver.findElement({css:'button[id="save"]'})),
        basic.SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Cannot find save button")});
    driver.sleep(basic.FAST_OPERATION);
    driver.findElement({css:'button[id="save"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on save button")});
    driver.sleep(basic.FAST_OPERATION);
    driver.findElement({css:'button[id="save"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on save button")});
}


basic.getDrivers().forEach(function (drv) {
    var driver = basic.getDriver(drv);

    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');
    create(driver, '01.01.2014 00:00:00');
    create(driver, '02.12.2015');
    create(driver, '12.07.2016');
    create(driver, ('0' + now.getDate()).slice(-2) + '.' + ('0' + (now.getMonth() + 1)).slice(-2) + '.' + now.getFullYear());
    driver.sleep(basic.EXTRA_SLOW_OPERATION)
    search(driver, "'v-s:created' == [2014-01-01T00:00:00, 2014-01-01T23:59:59]", 1);
    search(driver, "'v-s:created' == [2014-01-01T00:00:00, 2015-12-02T23:59:59]", 2);
    search(driver, "'v-s:created' == [2014-01-01T00:00:00, 2016-07-12T23:59:59]", 3);
    search(driver, "'v-s:created' == [2014-01-01T00:00:00, " +  now.getFullYear() + '-' + ('0' + (now.getMonth() + 1)).slice(-2) + '-'+ ('0' + now.getDate()).slice(-2) + "T23:59:59]", 4);

    driver.quit();
});