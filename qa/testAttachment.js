var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js');

basic.getDrivers().forEach(function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    //download
    basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
    driver.findElement({id:'submit'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'submit' button");});
    driver.sleep(basic.SLOW_OPERATION);
    driver.findElement({css:'a[href="#/cfg:Administrator"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Администратор'");});
    driver.findElement({css:'a[href="#/graph/cfg:Administrator"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'glyphicon-link'");});
    driver.findElement({id:'export-ttl'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'export-ttl' button");});

    //attach-save-check-delete-attach-save
    basic.openCreateDocumentForm(driver, 'Отчет', 'v-s:Report');
    var filePath = 'C:/Users/Administrator/Downloads/exported_graph.ttl';
    //var filePath = 'C:/Users/zugzug/Downloads/exported_graph.ttl';
    driver.executeScript("document.querySelector('strong[about=\"v-s:attachment\"]').scrollIntoView(true);");
    driver.findElement({css:'input[type="file"]'}).sendKeys(filePath)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot find this file");});
    basic.isVisible(driver, 'div[rel="v-s:attachment"]', basic.FAST_OPERATION);

    driver.executeScript("document.querySelector('button[id=\"save\"]').scrollIntoView(true);");
    driver.findElement({css:'button[id="save"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'save' button");});

    driver.executeScript("document.querySelector('strong[about=\"v-s:attachment\"]').scrollIntoView(true);");
    driver.findElement({css:'span[property="v-s:fileName"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'v-s:fileName'");});

    driver.executeScript("document.querySelector('button[id=\"edit\"]').scrollIntoView(true);");
    driver.findElement({css:'button[id="edit"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'edit' button");});

    driver.executeScript("document.querySelector('div[rel=\"v-s:attachment\"]').scrollIntoView(true);");
    basic.isVisible(driver,'div[rel="v-s:attachment"] div+div[id="rel-actions"] .button-delete', basic.SLOW_OPERATION);
    //driver.findElement({css:'div[rel="v-s:attachment"] div+div[id="rel-actions"] .button-delete'}).click()
    //    .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'button-delete' button");});
    driver.executeScript("document.elementFromPoint(675, 20).click();");


    driver.executeScript("document.querySelector('strong[about=\"v-s:attachment\"]').scrollIntoView(true);");
    driver.findElement({css:'input[type="file"]'}).sendKeys(filePath)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot find this file");});
    basic.isVisible(driver, 'div[rel="v-s:attachment"]', basic.FAST_OPERATION);

    driver.executeScript("document.querySelector('button[id=\"save\"]').scrollIntoView(true);");
    driver.findElement({css:'button[id="save"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'save' button");});
    
    driver.quit();
});
