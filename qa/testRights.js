var webdriver = require('selenium-webdriver'),
    person = require('./person.js'),
    basic = require('./basic.js');
    timeStamp = ''+Math.round(+new Date()/1000);

/**
 * Поиск
 *
 * @param somethingUnique - имя шаблона, документ которого ищется
 * @param count - количество документов, ожидаемое по окончанию поиска
 */

function search(driver, somethingUnique, count) {
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



basic.getDrivers().forEach (function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);

    //Создание документа пользователем с большими правами
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    var now = new Date();
    person.createPerson(driver, drv, 'Иванов', 'Иван', timeStamp, ('0' + now.getDate()).slice(-2) + '.' + ('0' + (now.getMonth() + 1)).slice(-2) + '.' + now.getFullYear());
    basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
    search(driver, timeStamp, 1);
    basic.logout(driver);

    //Проверка отсутсвия созданного документа и создание нового документа пользователем с меньшими правами
    basic.login(driver, 'bychinat', '123', '4', 'Администратор4');
    driver.findElement({id:'params-pill-ft'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'params-pill-ft' button");});
    driver.findElement({css:'div[typeof="v-fs:FulltextRequest"] input[id="fulltext"]'}).clear();
    driver.findElement({css:'div[typeof="v-fs:FulltextRequest"] input[id="fulltext"]'}).sendKeys('Персона');
    search(driver, timeStamp, 0);
    person.createPerson(driver, drv, 'Иванов', 'Иван', timeStamp + 1, ('0' + now.getDate()).slice(-2) + '.' + ('0' + (now.getMonth() + 1)).slice(-2) + '.' + now.getFullYear());
    driver.sleep(basic.FAST_OPERATION);
    driver.findElement({id:'menu'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button");});
    basic.isVisible(driver, 'li[id="menu"] li[resource="v-l:Inbox"]', basic.SLOW_OPERATION);
    driver.findElement({css:'li[id="menu"] li[resource="v-l:Inbox"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `inbox` button");});
    basic.logout(driver);

    //Проверка наличия созданного документа пользователем с меньшими правами
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
    search(driver, timeStamp + 1, 1);

    driver.quit();
});

