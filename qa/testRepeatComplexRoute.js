var basic = require('./basic.js'),
    complexRoute = require('./complexRoute.js'),
    assert = require('assert');


basic.getDrivers().forEach (function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    basic.openCreateDocumentForm(driver, 'Тестовый шаблон комплексного маршурута 2', 's-wf:ComplexRouteTest2');
    driver.executeScript("document.querySelector('#send').scrollIntoView(true)");
    driver.findElement({id:'send'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Send' button");});
    driver.sleep(basic.SLOW_OPERATION);
    driver.executeScript("document.querySelector('#save_and_start_process').scrollIntoView(true)");
    driver.sleep(basic.FAST_OPERATION);
    driver.findElement({id:'save_and_start_process'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'save_and_start_process' button");});
    driver.sleep(basic.FAST_OPERATION);
    basic.logout(driver);

    complexRoute.checkTask(driver, '1', 'bychinat', '123', '4', 'Администратор4');
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_finish'] ,['red'], 0);
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_finish'] ,['red'], 1);

    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    basic.openFulltextSearchDocumentForm(driver, 'Стартовая форма сети Комплексный маршрут', 's-wf:ComplexRouteStartForm');
    driver.findElement({id:'submit'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'submit' button");});
    driver.sleep(basic.FAST_OPERATION);
    driver.findElement({css:'a[typeof="s-wf:ComplexRouteTest2"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on document id");});
    driver.findElement({id:'send'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Send' button");});
    driver.sleep(basic.SLOW_OPERATION);
    driver.executeScript("document.querySelector('#save_and_start_process').scrollIntoView(true)");
    driver.sleep(basic.FAST_OPERATION);
    driver.findElement({id:'save_and_start_process'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'save_and_start_process' button");});
    driver.sleep(basic.FAST_OPERATION);
    basic.logout(driver);
    complexRoute.checkTask(driver, '1', 'bychinat', '123', '4', 'Администратор4');
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_finish'] ,['red'], 0);
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_finish'] ,['red'], 1);

    driver.quit();
});