var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js');

module.exports = {
    /**
     * Создание стартовой формы для сети со статусом документа оборота и проверка, что она запустилась
     * @param driver 
     * @param forNetValue - сеть, для которой создается стартовая форма
     * @hasStatusWorkflow - статус документаоборота
     * @phase - текущая фаза теста
    */
    createStartForm: function (driver, forNetValue, hasStatusWorkflow, phase) {
        // Create new start form
        basic.openCreateDocumentForm(driver, 'Стартовая форма', 'v-wf:StartForm', phase);
        driver.executeScript("document.querySelector('strong[about=\"v-wf:forNet\"]').scrollIntoView(true);")
            .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Cannot scroll to 'forNet' field");});
        basic.chooseFromDropdown(driver, 'v-wf:forNet', forNetValue, forNetValue, phase);
        driver.executeScript("document.querySelector('strong[about=\"v-wf:hasStatusWorkflow\"]').scrollIntoView(true);")
            .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Cannot scroll to 'hasStatusWorkflow' field");});
        basic.chooseFromDropdown(driver, 'v-wf:hasStatusWorkflow', hasStatusWorkflow, hasStatusWorkflow, phase);
        driver.executeScript("$('#save')[0].scrollIntoView(true);")
            .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Cannot scroll to save button");});
        basic.isEnabled(driver, '#save', basic.FAST_OPERATION, phase);
        basic.execute(driver, 'click', '#save', "****** PHASE#" + phase + " : ERROR = Cannot click on save button");
        //Смотрим на процесс
        driver.sleep(basic.FAST_OPERATION * 2);
        // driver.executeScript("location.reload();");
        driver.wait
        (
            webdriver.until.elementIsEnabled(driver.findElement({css:'div[rel="v-wf:isProcess"]'})),
            basic.SLOW_OPERATION
        ).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Cannot find 'isProcess' button");});
        driver.executeScript("document.querySelector('div[rel=\"v-wf:isProcess\"]').scrollIntoView(true);")
            .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Cannot scroll to isProcess field");});
        basic.execute(driver, 'click', 'div[rel=\"v-wf:isProcess\"] #label', "****** PHASE#" + phase + " : ERROR = Cannot click 'isProcess' button");
        basic.isVisible(driver, '.workflow-canvas-wrapper', basic.FAST_OPERATION, phase);
    }
};
