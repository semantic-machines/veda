var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js');

module.exports = {
    /**
     * Создание делегирования на должность Аналатика, вводом Персоны и Даты;
     * @param driver
     * @param valuteToSearch - Персона, которую надо искать для делегирования
     * @param valuteToChoose - Персона, которую надо выбрать для делегирования
     * @param phase - текущая фаза теста
    */
    createRequestDelegation: function (driver, valueToSearch, valueToChoose, delegatedPosition, phase) {
        basic.openCreateDocumentForm(driver, 'Заявка на делегирование для пользователя', 'v-s:RequestDelegationUser', phase);

        driver.executeScript("document.querySelector('#positions').scrollIntoView(true);");
        basic.execute(driver, 'click', 'div[id="positions"] input[id="' + delegatedPosition + '"]', "****** PHASE#" + phase + " : ERROR = Cannot click on 'Аналитик' position");

        basic.execute(driver, 'sendKeys', 'veda-control[rel="v-s:delegate"] input[id="fulltext"]', "****** PHASE#" + phase + " : ERROR = Cannot find attribute 'rel=v-s:delegate'", valueToSearch);
        driver.sleep(basic.FAST_OPERATION);
        driver.wait
        (
            function () {
                return driver.findElements({css:'veda-control[rel="v-s:delegate"] span[class="tt-dropdown-menu"] div[class="tt-dataset-dataset"] p'}).then(function (suggestions) {
                    return webdriver.promise.filter(suggestions, function(suggestion) {
                        return suggestion.getText().then(function(txt){
                            return txt.toLowerCase() === valueToChoose.toLowerCase();
                        });
                    }).then(function(x) { return x.length>0; });
                });
            },
            basic.SLOW_OPERATION
        ).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Cannot find '"+ valueToSearch +"' from dropdown");});

        driver.findElements({css:'veda-control[rel="v-s:delegate"] span[class="tt-dropdown-menu"] div[class="tt-dataset-dataset"] p'}).then(function (suggestions) {
            webdriver.promise.filter(suggestions, function(suggestion) {
                return suggestion.getText().then(function(txt){
                    if (valueToChoose === undefined) {
                        return txt.toLowerCase() == valueToSearch.toLowerCase();
                    } else {
                        return txt.toLowerCase() == valueToChoose.toLowerCase();
                    }
                });
            }).then(function(x) { x[0].click();});
        }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Cannot click on '" + valueToChoose + "' from dropdown");});

        basic.execute(driver, 'click', 'veda-control[property="v-s:dateFrom"] input[type="text"]',
            "****** PHASE#" + phase + " : ERROR = Cannot click on 'dateFrom' input");
        basic.execute(driver, 'click', 'veda-control[property="v-s:dateTo"] input[type="text"]',
            "****** PHASE#" + phase + " : ERROR = Cannot click on 'dateTo' input");
        driver.wait(basic.findUp(driver, '.glyphicon-chevron-up', 1, "****** PHASE#" + phase + " : ERROR = Cannot find 'glyphicon-chevron-up' button"),
            basic.FAST_OPERATION).then(function (result) {basic.clickUp(result)});
        driver.wait(basic.findUp(driver, '.glyphicon-chevron-up', 1, "****** PHASE#" + phase + " : ERROR = Cannot find 'glyphicon-chevron-up' button"),
            basic.FAST_OPERATION).then(function (result) {basic.clickUp(result)});
        driver.findElement({css:'veda-control[rel="v-s:delegate"] input[id="fulltext"]'}).click();
        driver.executeScript("document.querySelector('#save').scrollIntoView(true);");
        driver.sleep(basic.FAST_OPERATION);
        basic.execute(driver, 'click','#save', "****** PHASE#" + phase + " : ERROR = Cannot click on 'save' button");

    }
};