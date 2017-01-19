var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js');

function del(driver) {
    driver.findElement({css:'.delete-state'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'delete' button");});
    driver.switchTo().alert().accept();
}

function check(driver, element) {
    driver.findElements({css:''+element}).then(function(elements_arr){
        if (elements_arr.length > 0) {
            console.trace("Seems "+ element +" has not deleted");
            process.exit(1);
        }
    }).thenCatch(function (e) {basic.errorHandler(e, "Cannot find any "+ element);});
}

function equal(a, b, button) {
    if(a == b) {
        console.trace("Seems " + button + " does not work");
        process.exit(1);
    }
}

basic.getDrivers().forEach(function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);

    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    basic.openCreateDocumentForm(driver, 'Сеть', 'v-wf:Net');
    basic.isVisible(driver, '.workflow-canvas-wrapper', basic.FAST_OPERATION);
    driver.findElement({css:'.workflow-canvas-wrapper'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
    basic.isVisible(driver, 'span[about="v-wf:Net"]', basic.FAST_OPERATION);

    //Создание и удаление коннектора между двумя элементами
    new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-io-condition-input .ep'}), driver.findElement({css:'.state-io-condition-output'})).perform();
    driver.findElement({css:'svg[class="_jsPlumb_connector"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'connector' arrow");});
    del(driver);
    check(driver, 'svg[class="_jsPlumb_connector"]');

    // //Создание задачи, клонирование и удаление
    driver.findElement({css:'.create-task'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'create-task' button");});
    driver.findElement({css:'.state-task'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'state-task' button");});
    driver.findElement({css:'.copy-net-element'}).click()
        .thenCatch(function(e){basic.errorHandler(e, "Cannot click on 'copy-net-element' button");});
    del(driver);
    driver.findElement({css:'.state-task'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'state-task' button");});
    del(driver);
    check(driver, '.state-task');

    driver.findElement({css:'.create-condition'}).click()
        .thenCatch(function(e) {basic.errorHandler(e, "Cannot click 'create-condition' button");});
    driver.findElement({css:'.state-condition'}).click()
        .thenCatch(function(e) {basic.errorHandler(e, "Cannot click 'state-condition' button");});
    var a = " ", b = "";
    driver.findElement({css:'div[id="workflow-canvas"]'}).getCssValue("transform").then(function (state) {a = state;})
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
    driver.findElement({css:'.zoom-out'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'zoom-out' button");});
    driver.findElement({css:'div[id="workflow-canvas"]'}).getCssValue("transform").then(function (state) {b == state;})
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
    equal(a, b, 'zoom-out');

    driver.findElement({css:'.zoom-in'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'zoom-in' button");});
    equal(a, b, 'zoom-in');

    driver.findElement({css:'.zoom-out'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'zoom-out' button");});
    driver.findElement({css:'.zoom-out'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'zoom-out' button");});
    driver.findElement({css:'.zoom-default'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'zoom-default' button");});
    driver.findElement({css:'div[id="workflow-canvas"]'}).getCssValue("transform").then(function (state) {b == state;})
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
    equal(a, b, 'zoom-default');

    driver.findElement({css:'button[id="full-width"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'full-width' button");});
    driver.findElement({css:'div[id="props-col"]'}).isDisplayed().then(function (state) {
        if (state === true) {
            console.trace("Seems 'button[id=full-width]' does not work");
            process.exit(1);
        }
    });

    driver.findElement({css:'#workflow-save-button'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'save net' button");});
    driver.sleep(basic.SLOW_OPERATION);
    driver.findElements({css:'h4[about="v-fc:ChooseType"]'}).then(function(elements_arr) {
        if (elements_arr.length > 0) {
            console.trace("Seems save-button does not work");
            process.exit(1);
        }
    });

    driver.quit();
});
