var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    startForm = require('./startForm.js');



function choose(driver, type, valueToSearch, valueToChoose) {
    driver.findElement({css:'veda-control[class="'+ type +' fulltext dropdown create properties-editor"] input[id="fulltext"]'}).sendKeys(valueToSearch)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot find attribute " + type + "");});
    driver.sleep(basic.FAST_OPERATION);
    driver.wait
    (
        function () {
            return driver.findElements({css:'veda-control[class="'+ type +' fulltext dropdown create properties-editor"] span[class="tt-dropdown-menu"] div[class="tt-dataset-dataset"] p'}).then(function (suggestions) {
                return webdriver.promise.filter(suggestions, function(suggestion) {
                    return suggestion.getText().then(function(txt){
                        return txt.toLowerCase() === valueToChoose.toLowerCase();
                    });
                }).then(function(x) { return x.length>0; });
            });
        },
        basic.SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Cannot find '"+ valueToSearch +"' from dropdown");});


    // Кликаем на запрашиваемый тип в выпавшем списке
    driver.findElements({css:'veda-control[class="'+ type +' fulltext dropdown create properties-editor"] span[class="tt-dropdown-menu"] div[class="tt-dataset-dataset"] p'}).then(function (suggestions) {
        webdriver.promise.filter(suggestions, function(suggestion) {
            return suggestion.getText().then(function(txt){
                if (valueToChoose === undefined) {
                    return txt.toLowerCase() == valueToSearch.toLowerCase();
                } else {
                    return txt.toLowerCase() == valueToChoose.toLowerCase();
                }
            });
        }).then(function(x) { x[0].click();});
    }).thenCatch(function (e) {basic.errorHandler(e, "Cannot click on '"+ valueToChoose +"' from dropdown");});
}

module.exports = {
    startNet: function (driver, timeStamp) {
        basic.openCreateDocumentForm(driver, 'Сеть', 'v-wf:Net');
        basic.isVisible(driver, '.workflow-canvas-wrapper', basic.FAST_OPERATION);
        driver.findElement({css:'.workflow-canvas-wrapper'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
        basic.isVisible(driver, 'span[about="v-wf:Net"]', basic.FAST_OPERATION);
        driver.findElement({css:'#props-col [about="rdfs:label"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net rdfs:label");});
        driver.findElement({css:'#VClabel input'}).sendKeys(timeStamp)
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill rdfl:label in net properties");});
    },

    chooseFromDropdown: function (driver, type, valueToSearch, valueToChoose) {
        choose(driver, type, valueToSearch, valueToChoose);
    },

    createTask: function(driver, toFind, taskExecutor) {
        driver.findElement({css:'.create-task'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'create-task' button");});
        if (taskExecutor != "false") {
            driver.findElement({css:'.state-task'}).click()
                .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'state-task' button");});
            driver.executeScript("$('span[about=\"v-wf:executor\"]')[0].scrollIntoView(true);");
            driver.findElement({css:'span[about="v-wf:executor"]'}).click()
                .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'executor' field ");});
            driver.findElement({css:'veda-control[class="VCexecutor fulltext dropdown create properties-editor"]'}).click()
                .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'VCexecutor' field ");});
            choose(driver, 'VCexecutor', toFind, taskExecutor);
        }
    },

    connectNet: function (driver, hasTask) {
        if (hasTask === "false") {
            new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-io-condition-input .ep'}),
                driver.findElement({css:'.state-io-condition-output'})).perform();
        }
        if (hasTask === "true") {
            new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-io-condition-input .ep'}),
                driver.findElement({css:'.state-task'})).perform();
            new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-task .ep'}),
                driver.findElement({css:'.state-io-condition-output'})).perform();
        }

    },

    saveNet: function(driver) {
        driver.findElement({css:'#workflow-save-button'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click save net");});
        driver.sleep(basic.FAST_OPERATION);//+
    },

    checkNet: function(driver, timeStamp, input, task, output) {
        startForm.createStartForm(driver, timeStamp, 'Ожидает отправки');
        driver.findElement({css:'.workflow-canvas-wrapper'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
        driver.findElement({css:'.state-io-condition-input[colored-to="' + input + '"]'})
            .thenCatch(function (e) {basic.errorHandler(e, "Seems 'input' condition is not located/" + input);});
        if (task != "-") {
            driver.findElement({css: '.state-task[colored-to="' + task + '"]'})
                .thenCatch(function (e) {basic.errorHandler(e, "Seems 'state-task' condition is not located/" + task);});
        }
        if (output != "-") {
            driver.findElement({css:'.state-io-condition-output[colored-to="' + output + '"]'})
                .thenCatch(function (e) {basic.errorHandler(e, "Seems 'output' condition is not located/" + output);});
        }
    }
};

