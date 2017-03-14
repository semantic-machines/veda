var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js');

module.exports = {
    /**
     * Создание стартовой формы для сети со статусом документа оборота и проверка, что она запустилась
     * @param driver 
     * @param forNetValue - сеть, для которой создается стартовая форма
     * @hasStatusWorkflow - статус документаоборота
    */
    createStartForm: function (driver, forNetValue, hasStatusWorkflow) {
        // Create new start form
        basic.openCreateDocumentForm(driver, 'Стартовая форма', 'v-wf:StartForm');
        driver.executeScript("document.querySelector('strong[about=\"v-wf:forNet\"]').scrollIntoView(true);");
        basic.chooseFromDropdown(driver, 'v-wf:forNet', forNetValue, forNetValue);
        driver.executeScript("document.querySelector('strong[about=\"v-wf:hasStatusWorkflow\"]').scrollIntoView(true);");
        basic.chooseFromDropdown(driver, 'v-wf:hasStatusWorkflow', hasStatusWorkflow, hasStatusWorkflow);
        driver.executeScript("$('#save')[0].scrollIntoView(true);");
        basic.isEnabled(driver, '#save', basic.FAST_OPERATION);
        basic.execute(driver, 'click', '#save', "Cannot click on save button");
        //Смотрим на процесс
        driver.executeScript("location.reload();");
        driver.wait
        (
            webdriver.until.elementIsEnabled(driver.findElement({css:'div[rel="v-wf:isProcess"]'})),
            basic.SLOW_OPERATION
        ).thenCatch(function (e) {basic.errorHandler(e, "Cannot find 'isProcess' button");});
        driver.executeScript("document.querySelector('div[rel=\"v-wf:isProcess\"]').scrollIntoView(true);");
        basic.execute(driver, 'click', 'div[rel=\"v-wf:isProcess\"] #label', "Cannot click 'isProcess' button");
        basic.isVisible(driver, '.workflow-canvas-wrapper', basic.FAST_OPERATION);
    }
};
