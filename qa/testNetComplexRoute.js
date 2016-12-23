var basic = require('./basic.js');

function openMsg(driver) {
    driver.findElement({id:'menu'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button");});
    basic.isVisible(driver, 'li[id="menu"] li[resource="v-l:Inbox"]', basic.SLOW_OPERATION);
    driver.findElement({css:'li[id="menu"] li[resource="v-l:Inbox"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `exit` button");});
    var container = driver.findElement({id:'main'});
    var content = container.innerHTML;
    container.innerHTML = content;
    driver.findElement({css:'a[property="rdfs:label"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Согласовать' button");});
    driver.executeScript("document.querySelector('#send').scrollIntoView(true)");
    driver.findElement({id:'send'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Send' button");});
}

basic.getDrivers().forEach (function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    basic.openCreateDocumentForm(driver, 'Тестовый шаблон комплексного маршурута', 's-wf:ComplexRouteTest');
    driver.executeScript("document.querySelector('#send').scrollIntoView(true)");
    driver.findElement({id:'send'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Send' button");});

    driver.executeScript("document.querySelector('#save_and_start_process').scrollIntoView(true)");
    driver.findElement({id:'save_and_start_process'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'save_and_start_process' button");});

    driver.sleep(basic.FAST_OPERATION);
    openMsg(driver);
    driver.sleep(basic.SLOW_OPERATION);
    basic.logout(driver);

    basic.login(driver, 'bychinat', '123', '4', 'Администратор4');
    openMsg(driver);

    basic.logout(driver);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    basic.openFulltextSearchDocumentForm(driver, 'Стартовая форма сети Комплексный маршрут', 's-wf:ComplexRouteStartForm');
    driver.findElement({id:'submit'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'submit' button");});
    driver.findElement({css:'span[rel="v-wf:isProcess"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'экземпляр маршрута :Комплексный маршрут' button");});
    driver.sleep(basic.FAST_OPERATION);
    driver.findElement({css:'.glyphicon-share-alt'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'glyphicon-share-alt'");});
    driver.findElement({css:'.state-io-condition-output[colored-to="red"]'})
        .thenCatch(function (e) {basic.errorHandler(e, "Seems 'output' is not red");});


    driver.quit();
});

