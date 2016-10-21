var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    startForm = require('./startForm.js');

module.exports = {
    startNet: function (driver, timeStamp) {
        basic.openCreateDocumentForm(driver, 'Сеть', 'v-wf:Net');
        driver.sleep(basic.FAST_OPERATION);
        driver.findElement({css:'.workflow-canvas-wrapper'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
        driver.sleep(basic.FAST_OPERATION);
        driver.findElement({css:'#props-col [about="rdfs:label"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on route rdfs:label");});
        driver.findElement({css:'#VClabel input'}).sendKeys(timeStamp)
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill rdfl:label in net properties");});
    },

    createTask: function(driver, toFind, taskExecutor) {
        driver.findElement({css:'.create-task'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click add task");});
        if (taskExecutor != "false") {
            driver.findElement({css:'.state-task'}).click()
                .thenCatch(function (e) {basic.errorHandler(e, "Cannot click 'state-task'");});
            driver.executeScript("$('span[about=\"v-wf:executor\"]')[0].scrollIntoView(true);");
            driver.findElement({css:'span[about="v-wf:executor"]'}).click()
                .thenCatch(function (e) {basic.errorHandler(e, "Cannot click 'executor' field ");});
            driver.findElement({css:'veda-control[class="VCexecutor fulltext dropdown create properties-editor"]'}).click()
                .thenCatch(function (e) {basic.errorHandler(e, "Cannot click 'VCexecutor' field ");});
            driver.findElement({css:'veda-control[class="VCexecutor fulltext dropdown create properties-editor"] input[id="fulltext"]'}).sendKeys(toFind)
                .thenCatch(function (e) {basic.errorHandler(e, "Cannot find attribute 'VCexecutor'");});
            driver.wait
            (
                function () {
                    return driver.findElements({css:'veda-control[class="VCexecutor fulltext dropdown create properties-editor"] span[class="tt-dropdown-menu"] div[class="tt-dataset-dataset"] p'}).then(function (suggestions) {
                        return webdriver.promise.filter(suggestions, function(suggestion) {
                            return suggestion.getText().then(function(txt){
                                return txt.toLowerCase() === taskExecutor.toLowerCase();
                            });
                        }).then(function(x) { return x.length > 0;});
                    });
                },
                basic.FAST_OPERATION
            ).thenCatch(function (e) {basic.errorHandler(e, "Cannot find 'Андрей Бычин : Аналитик' from dropdown");});

            driver.findElements({css:'veda-control[class="VCexecutor fulltext dropdown create properties-editor"] span[class="tt-dropdown-menu"] div[class="tt-dataset-dataset"] p'}).then(function (suggestions) {
                webdriver.promise.filter(suggestions, function(suggestion) {
                    return suggestion.getText().then(function(txt){
                        if (taskExecutor === undefined) {
                            return txt.toLowerCase() == toFind.toLowerCase();
                        } else {
                            return txt.toLowerCase() == taskExecutor.toLowerCase();
                        }
                    });
                }).then(function(x) { x[0].click();});
            }).thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Андрей Бычин :  Аналитик' from dropdown");});
        }
    },

    connectNet: function (driver, hasTask) {
        if (hasTask === "false") {
            new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-io-condition-input .ep'}), driver.findElement({css:'.state-io-condition-output'})).perform();
        }
        if (hasTask === "true") {
            new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-io-condition-input .ep'}), driver.findElement({css:'.state-task'})).perform();
            new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-task .ep'}), driver.findElement({css:'.state-io-condition-output'})).perform();
        }

    },

    saveNet: function(driver) {
        driver.findElement({css:'#workflow-save-button'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click save net");});
        driver.sleep(basic.FAST_OPERATION);
    },

    checkNet: function(driver, timeStamp, input, task, output) {
        startForm.createStartForm(driver, timeStamp, 'Ожидает отправки');
        driver.findElement({css:'.workflow-canvas-wrapper'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
        driver.findElement({css:'.state-io-condition-input[colored-to="' + input + '"]'})
            .thenCatch(function (e) {basic.errorHandler(e, "Seems 'input' button is not located/" + input);});
        driver.sleep(basic.FAST_OPERATION);
        if (task != "-") {
            driver.findElement({css: '.state-task[colored-to="' + task + '"]'})
                .thenCatch(function (e) {basic.errorHandler(e, "Seems 'state-task' button is not located/" + task);});
        }
        driver.sleep(basic.FAST_OPERATION);
        if (output != "-") {
            driver.findElement({css:'.state-io-condition-output[colored-to="' + output + '"]'})
                .thenCatch(function (e) {basic.errorHandler(e, "Seems 'state-task' button is not located/" + output);});
        }
        driver.sleep(basic.FAST_OPERATION);
    }
};

