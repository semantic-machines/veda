var basic = require('./basic.js'),
    complexRoute = require('./complexRoute.js'),
    assert = require('assert');

basic.getDrivers().forEach (function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    basic.openCreateDocumentForm(driver, 'Тестовый шаблон комплексного маршурута', 's-wf:ComplexRouteTest');
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

    //coordination1

    complexRoute.acceptTask(driver, '0', '-', '-', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');
    complexRoute.checkRouteStatus(driver, ['s-wf:cr_c1', 's-wf:cr_c2'], ['green', 'red'], 1);

    //coordination2

    complexRoute.acceptTask(driver, '0', '-', '-', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '1', '+', '-', 'bychinat', '123', '4', 'Администратор4');
    complexRoute.checkRouteStatus(driver, ['s-wf:cr_c2', 's-wf:cr_rework'], ['green', 'red'], 1);
    complexRoute.acceptTask(driver, '1', '-', '-', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');
    complexRoute.acceptTask(driver, '0', '-', '-', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.checkRouteStatus(driver, ['s-wf:cr_review', 's-wf:cr_instruction', 's-wf:cr_examination'],
        ['red', 'red', 'red'], 1);

    //review, instruction, examination -> instruction2
    complexRoute.checkTask(driver, '3', 'bychinat', '123', '4', 'Администратор4');
    complexRoute.checkTask(driver, '0', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '0', '-', '-',  'bychinat', '123', '4', 'Администратор4');
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_review', 's-wf:cr_instruction', 's-wf:cr_examination', 's-wf:cr_instruction2'],
    //    ['red', 'red', 'green', 'red'], 1);
    complexRoute.checkTask(driver, '0', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');
    // complexRoute.checkRouteStatus(driver, ['s-wf:cr_review', 's-wf:cr_instruction', 's-wf:cr_examination', 's-wf:cr_instruction2'],
    //     ['green', 'red', 'green', 'red'], 1);
    complexRoute.checkTask(driver, '0', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');
    complexRoute.checkRouteStatus(driver, ['s-wf:cr_review', 's-wf:cr_instruction', 's-wf:cr_examination', 's-wf:cr_instruction2'],
        ['green', 'green', 'green', 'red'], 1);

    //
    complexRoute.checkTask(driver, '1', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '3', '+', '+', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '4', '+', '-', 'bychinat', '123', '4', 'Администратор4');
    complexRoute.acceptTask(driver, '1', '+', '-', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '2', '+', '-', 'bychinat', '123', '4', 'Администратор4');
    complexRoute.acceptTask(driver, '1', '+', '-', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');
    complexRoute.checkRouteStatus(driver, ['s-wf:cr_finish'] ,['red'], 0);

    //
    complexRoute.acceptTask(driver, '2', '+', '+', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '1', '+', '-', 'bychinat', '123', '4', 'Администратор4');
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');

    complexRoute.checkRouteStatus(driver, ['s-wf:cr_instruction2', 's-wf:cr_finish'],
        ['red' , 'red'], 1);

    driver.quit();
});

