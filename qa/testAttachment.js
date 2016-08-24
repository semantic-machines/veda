var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js');

basic.getDrivers().forEach(function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');

    driver.findElement({id:"menu"}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button");});
    driver.wait
    (
        webdriver.until.elementIsVisible(driver.findElement({css:'li[id="menu"] li[resource="v-l:Graph"]'})),
        basic.FAST_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Seems there is no 'search' button inside menu");});
    driver.findElement({css:'li[id="menu"] li[resource="v-l:Graph"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'search' button");});
    driver.findElement({id:'export-ttl'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'export-ttl' button");});

    basic.openCreateDocumentForm(driver, 'Отчет', 'v-s:Report');
    var filePath = 'C:/Users/Administrator/Downloads/exported_graph.ttl';
    //var filePath = 'C:/Users/zugzug/Downloads/exported_graph.ttl';
    //driver.executeScript("document.querySelector('strong[about=\"v-s:owner\"]').scrollIntoView(true);");
    driver.executeScript("document.querySelector('input[id=\"file\"]').scrollIntoView(true);");
    driver.findElement({css:'input[type="file"]'}).sendKeys(filePath)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot find this file");});
    driver.sleep(basic.SLOW_OPERATION);
    
    driver.executeScript("document.querySelector('span[property=\"v-s:fileName\"]').scrollIntoView(true);");
    driver.findElement({css:'span[property="v-s:fileName"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'v-s:fileName'");});
    driver.sleep(basic.SLOW_OPERATION);

    driver.executeScript("document.querySelector('div[rel=\"v-s:attachment\"]').scrollIntoView(true);");
    driver.findElement({css:'div[rel="v-s:attachment"] div+div[id="rel-actions"] button'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'button-delete' button");});
    driver.sleep(basic.FAST_OPERATION);
    
    driver.executeScript("document.querySelector('button[id=\"save\"]').scrollIntoView(true);");
    driver.findElement({css:'button[id="save"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'save' button");});
    
    driver.quit();
});