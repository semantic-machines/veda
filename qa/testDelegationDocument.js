var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    person = require('./person.js'),
    delegationRequest = require('./delegationRequest.js'),
    timeStamp = ''+Math.round(+new Date()/1000);

function check(driver, somethingUnique, count) {
    driver.findElement({id:'params-pill-ft'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'params-pill-ft' button");});
    driver.findElement({css:'h4[about="v-fs:EnterQuery"]+div[class="form-group"] input'}).sendKeys(somethingUnique)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot input search request");});
    driver.wait
    (
        function () {
            driver.findElement({css:'h4[about="v-fs:EnterQuery"]+div[class="form-group"] button[id="submit"]'}).click();
            driver.sleep(basic.FAST_OPERATION);
            return driver.findElement({css:'span[href="#params-ft"]+span[class="badge"]'}).getText().then(function (txt) {
                return txt == count;
            });
        },
        basic.EXTRA_SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Number of documents is incorrect, expected: " + count);});
}

basic.getDrivers().forEach(function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);

    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    var now = new Date();
    person.createPerson(driver, drv, 'Bourne', 'Jason', timeStamp,
        ('0' + now.getDate()).slice(-2) + '.' + ('0' + (now.getMonth() + 1)).slice(-2) + '.' + now.getFullYear());

    basic.logout(driver);
    basic.login(driver, 'bychinat', '123', '4', 'Администратор4');
    basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
    check(driver,timeStamp, 0);

    basic.logout(driver);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    delegationRequest.createRequestDelegation(driver, 'Администратор4', 'Администратор4 : Аналитик');
    driver.findElement({css:'a[href="#/v-l:Welcome"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Welcome' button")});
    driver.sleep(basic.FAST_OPERATION);

    basic.logout(driver);
    basic.login(driver, 'bychinat', '123', '4', 'Администратор4');
    basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
    check(driver, timeStamp, 1);

    driver.quit();
});
