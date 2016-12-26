var basic = require('./basic.js'),
    timeStamp = ''+Math.round(+new Date()/1000);

function openMsg(driver, decision) {
    driver.findElement({id:'menu'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button");});
    basic.isVisible(driver, 'li[id="menu"] li[resource="v-l:Inbox"]', basic.SLOW_OPERATION);
    driver.findElement({css:'li[id="menu"] li[resource="v-l:Inbox"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `inbox` button");});
    driver.sleep(basic.EXTRA_SLOW_OPERATION);
    driver.navigate().refresh();
    driver.sleep(4000);
    var container = driver.findElement({id:'main'});
    var content = container.innerHTML;
    container.innerHTML = content;
    driver.findElement({css:'a[property="rdfs:label"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Согласовать' button");});
    if(decision === '0') {
        driver.findElement({css:'div[class="radio decision"] input[value="0"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on '0' decision");});
    }
    if(decision === '1') {
        driver.findElement({css:'div[class="radio decision"] input[value="1"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on '1' decision");});
        driver.findElement({css:'veda-control[property="rdfs:comment"] div textarea'}).sendKeys(timeStamp)
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'comment'");});
    }

    driver.sleep(basic.FAST_OPERATION);
    driver.executeScript("document.querySelector('#send').scrollIntoView(true)");
    driver.findElement({id:'send'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Send' button");});
    driver.findElement({css:'a[href="#/v-l:Welcome"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Welcome' button")});
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

    //coordination1

    openMsg(driver, '0');
    basic.logout(driver);

    basic.login(driver, 'bychinat', '123', '4', 'Администратор4');
    openMsg(driver, '0');
    driver.sleep(basic.FAST_OPERATION);
    basic.logout(driver);

    //coordination2

    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    openMsg(driver, '0');
    driver.sleep(basic.FAST_OPERATION);
    basic.logout(driver);

    basic.login(driver, 'bychinat', '123', '4', 'Администратор4');
    openMsg(driver, '1');
    driver.sleep(basic.FAST_OPERATION);
    basic.logout(driver);

    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    openMsg(driver, '1');
    driver.sleep(basic.FAST_OPERATION);
    basic.logout(driver);

    basic.login(driver, 'bychinat', '123', '4', 'Администратор4');
    openMsg(driver, '0');
    driver.sleep(basic.FAST_OPERATION);
    basic.logout(driver);

    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    openMsg(driver, '0');

    //check
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

