var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js');

module.exports = {
    createStartForm: function (driver, forNetValue, hasStatusWorkflow) {
        // Create new start form
        basic.openCreateDocumentForm(driver, 'Стартовая форма', 'v-wf:StartForm');
        driver.executeScript("document.querySelector('strong[about=\"v-wf:forNet\"]').scrollIntoView(true);");
        basic.chooseFromDropdown(driver, 'v-wf:forNet', forNetValue, forNetValue);
        driver.executeScript("document.querySelector('strong[about=\"v-wf:hasStatusWorkflow\"]').scrollIntoView(true);");
        basic.chooseFromDropdown(driver, 'v-wf:hasStatusWorkflow', hasStatusWorkflow, hasStatusWorkflow);
        driver.executeScript("$('#save')[0].scrollIntoView(true);");
        basic.isEnabled(driver, '#save', basic.FAST_OPERATION);
        driver.findElement({css:'#save'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on save button");});
        //Смотрим на процесс
        driver.executeScript("location.reload();");
        driver.wait
        (
            webdriver.until.elementIsEnabled(driver.findElement({css:'div[rel="v-wf:isProcess"]'})),
            basic.SLOW_OPERATION
        ).thenCatch(function (e) {basic.errorHandler(e, "Cannot find 'isProcess' button");});
        driver.executeScript("document.querySelector('div[rel=\"v-wf:isProcess\"]').scrollIntoView(true);");
        driver.findElement({css:'div[rel=\"v-wf:isProcess\"] #label'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click 'isProcess' button");});
        basic.isVisible(driver, '.workflow-canvas-wrapper', basic.FAST_OPERATION);
    }
};
