var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    startForm = require('./startForm.js');

/**
 * Поиск и выбор значение в аттрибуте
 * @param driver
 * @param type - аттрибут
 * @param valueToSearch - значение, которое необходимо искать
 * @param valueToChoose - значение, которое необходимо выбрать
*/

function choose(driver, type, valueToSearch, valueToChoose) {
    basic.execute(driver, 'sendKeys', 'veda-control[class="'+ type +' fulltext dropdown create properties-editor"] input[id="fulltext"]',
        "Cannot find attribute " + type, valueToSearch);
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
    /**
     * Открытие редактора создания сети и ввод ее названия
     * @param driver
     * @param timeStamp - название сети
    */
    startNet: function (driver, timeStamp) {
        basic.openCreateDocumentForm(driver, 'Сеть', 'v-wf:Net');
        basic.isVisible(driver, '.workflow-canvas-wrapper', basic.FAST_OPERATION);
        var startPoint = driver.findElement({css:'.glyphicon-play'});
        var actionSequence = webdriver.ActionSequence;        
        var act = new actionSequence(driver);
        act.mouseMove(startPoint, {x: 200, y: 0}).click().perform();
        basic.zoomToNormal(driver);
        basic.isVisible(driver, 'span[about="v-wf:Net"]', basic.FAST_OPERATION);
        basic.execute(driver, 'click', '#props-col [about="rdfs:label"]', "Cannot click on net rdfs:label");
        basic.execute(driver, 'sendKeys', '#VClabel input', "Cannot fill rdfl:label in net properties", timeStamp);
        driver.sleep(basic.FAST_OPERATION * 2);
    },
    /**
     * Запуск функции choose 
     * @param driver
     * @param type - аттрибут
     * @param valueToSearch - значение, которое нужно искать
     * @param valueToChoose - значение, которое нужно выбрать
    */
    chooseFromDropdown: function (driver, type, valueToSearch, valueToChoose) {
        choose(driver, type, valueToSearch, valueToChoose);
    },
    /**
     * Создание задачи с исполнителем
     * @param driver
     * @param toFind - исполнитель, которого нужно искать
     * @param taskExecutor - исполнитель, которого нужно выбрать
    */
    createTask: function(driver, toFind, taskExecutor) {
        basic.execute(driver, 'click', '.create-task', "Cannot click on 'create-task' button");
        if (taskExecutor != "false") {
            basic.execute(driver, 'click', '.state-task', "Cannot click on 'state-task' button");
            driver.executeScript("$('span[about=\"v-wf:executor\"]')[0].scrollIntoView(true);");
            basic.execute(driver, 'click', 'span[about="v-wf:executor"]', "Cannot click on 'executor' field ");
            basic.execute(driver, 'click', 'veda-control[class="VCexecutor fulltext dropdown create properties-editor"]', "Cannot click on 'VCexecutor' field ");
            choose(driver, 'VCexecutor', toFind, taskExecutor);
        }
    },

    /**
     * Соединение элементов в сети
     * @param driver
     * @param hasTask - имеется ли задача в сети
    */
    connectNet: function (driver, hasTask) {
        if (hasTask === "false") {
            new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-io-condition-input .ep'}),
                driver.findElement({css:'.glyphicon-stop'})).perform();
        }
        if (hasTask === "true") {
            new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-io-condition-input .ep'}),
                driver.findElement({css:'.state-task'})).perform();
            new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-task .ep'}),
                driver.findElement({css:'.glyphicon-stop'})).perform();
        }

    },

    /**
     * Сохранение сети
     * @param driver
    */
    saveNet: function(driver) {
        basic.execute(driver, 'click', '#workflow-save-button', "Cannot click save net");
        driver.sleep(basic.FAST_OPERATION);//+
    },

    /**
     * Проверка состояния сети после исполнения
     * @param driver
     * @param timeStamp - название сети
     * @param input - состояние входа
     * @param task - состояние задачи
     * @param output - состояние выхода
    */
    checkNet: function(driver, timeStamp, input, task, output) {
        startForm.createStartForm(driver, timeStamp, 'Ожидает отправки');
        basic.execute(driver, 'click', '.workflow-canvas-wrapper', "Cannot click on net canvas");
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

