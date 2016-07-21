var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js');

module.exports = {
    createStartForm: function (driver, forNetValue, hasStatusWorkflow) {
        // Create new start form
        basic.openCreateDocumentForm(driver, 'Стартовая форма', 'v-wf:StartForm');
        driver.executeScript("document.querySelector('strong[about=\"v-wf:forNet\"]').scrollIntoView(true);");
        basic.chooseFromDropdown(driver, 'v-wf:forNet', forNetValue, forNetValue);
        driver.executeScript("document.querySelector('strong[about=\"v-s:hasStatusWorkflow\"]').scrollIntoView(true);");
        basic.chooseFromDropdown(driver, 'v-s:hasStatusWorkflow', hasStatusWorkflow, hasStatusWorkflow);
        driver.executeScript("$('#save')[0].scrollIntoView(true);");
        driver.wait
        (
            webdriver.until.elementIsEnabled(driver.findElement({css:'#save'})),
            basic.FAST_OPERATION
        ).thenCatch(function (e) {basic.errorHandler(e, "Cannot find save button")});
        driver.findElement({css:'#save'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on save button")});
        //Смотрим на процесс
        driver.sleep(basic.FAST_OPERATION);
        driver.executeScript("document.querySelector('span[typeof=\"v-wf:Process\"]').scrollIntoView(true);");
        driver.wait
        (
            webdriver.until.elementIsEnabled(driver.findElement({css:'span[typeof="v-wf:Process"]'})),
            basic.FAST_OPERATION
        ).thenCatch(function (e) {basic.errorHandler(e, "Cannot find 'isProcess' button")});
        driver.sleep(basic.FAST_OPERATION);
        driver.findElement({css:'span[typeof="v-wf:Process"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click 'isProcess' button")});
    }
}
