var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    timeStamp = ''+Math.round(+new Date()/1000),
    startForm = require('./startForm.js');

basic.getDrivers().forEach (function (drv) {
    var driver = basic.getDriver(drv);

    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');
    
    // Create new simple net
    basic.openCreateDocumentForm(driver, 'Сеть', 'v-wf:Net');
    driver.sleep(basic.FAST_OPERATION);
    driver.findElement({css:'.workflow-canvas-wrapper'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
    driver.sleep(basic.FAST_OPERATION);
    driver.findElement({css:'#props-col [about="rdfs:label"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on route rdfs:label");});
    driver.findElement({css:'#VClabel input'}).sendKeys(timeStamp)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill rdfl:label in net properties");});
    new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-io-condition-input .ep'}), driver.findElement({css:'.state-io-condition-output'})).perform();
    driver.findElement({css:'#workflow-save-button'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click save net");});
    driver.sleep(basic.FAST_OPERATION);

    startForm.createStartForm(driver, timeStamp, 'Ожидает отправки');
    driver.findElement({css:'.workflow-canvas-wrapper'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
    driver.findElement({css:'.state-io-condition-input[colored-to="red"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'input' button");});
    driver.sleep(basic.FAST_OPERATION);
    //driver.findElement({css:'.w state-io-condition state-io-condition-output [colored-to="red"]'}).click()
    driver.findElement({css:'.state-io-condition-output[colored-to="red"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'output' button");});
    driver.sleep(basic.FAST_OPERATION);
    driver.quit();
});
