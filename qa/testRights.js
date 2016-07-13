var webdriver = require('selenium-webdriver'),
    person = require('./person.js'),
    basic = require('./basic.js');
    timeStamp = ''+Math.round(+new Date()/1000);

function logout(driver) {
    driver.findElement({id:'menu'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button")});
    driver.wait
    (
        webdriver.until.elementIsVisible(driver.findElement({css:'li[id="menu"] li[resource="v-l:Exit"]'})),
        basic.FAST_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Seems there is no `exit` button inside menu")});
    driver.findElement({css:'li[id="menu"] li[resource="v-l:Exit"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `exit` button")});
    driver.sleep(basic.FAST_OPERATION);
    driver.findElement({css:'input[id="login"'}).clear()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot clear 'login' field")});
    driver.findElement({css:'input[id="password"'}).clear()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot clear 'password' field")});
}

/**
 * Поиск
 *
 * @param somethingUnique - имя шаблона, документ которого ищется
 * @param count - количество документов, ожидаемое по окончанию поиска
 */

function search(driver, somethingUnique, count) {
    driver.findElement({id:'params-pill-ft'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'params' button")});
    driver.findElement({css:'h4[about="v-fs:EnterQuery"]+div[class="form-group"] input'}).sendKeys(somethingUnique)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot input search request")});;

    driver.sleep(basic.EXTRA_SLOW_OPERATION);
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
    ).thenCatch(function (e) {basic.errorHandler(e, "Number of documents is incorrect, expected: " + count)});
}



basic.getDrivers().forEach (function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);

    //Создание документа пользователем с большими правами
    basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');
    person.createPerson(driver, drv, timeStamp);
    basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
    search(driver, timeStamp, 1);
    logout(driver);

    //Проверка отсутсвия созданного документа и создание нового документа пользователем с меньшими правами
    basic.login(driver, 'bychinat', '123', 'Андрей', 'Бычин');
    basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
    search(driver, timeStamp, 0);
    person.createPerson(driver, drv, timeStamp + 1);
    logout(driver);

    //Проверка наличия созданного документа пользователем с меньшими правами
    basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');
    basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
    search(driver, timeStamp + 1, 1);

    driver.quit();
});

