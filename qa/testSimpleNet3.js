var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    timeStamp = ''+Math.round(+new Date()/1000),
    startForm = require('./startForm.js');

basic.getDrivers().forEach (function (drv) {
    var driver = basic.getDriver(drv);

    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    // Create new simple net
    basic.openCreateDocumentForm(driver, 'Сеть', 'v-wf:Net');
    // Create new simple net
    driver.findElement({css:'.workflow-canvas-wrapper'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
    driver.sleep(basic.FAST_OPERATION);
    driver.findElement({css:'#props-col [about="rdfs:label"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on route rdfs:label");});
    driver.findElement({css:'#VClabel input'}).sendKeys(timeStamp)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill rdfl:label in net properties");});
    driver.findElement({css:'.create-task'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click add task");});
    driver.findElement({css:'div[class="w state-task split-join split-no join-no jsplumb-draggable jsplumb-droppable"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click 'state-task'");});
    driver.executeScript("$('span[about=\"v-wf:executor\"]')[0].scrollIntoView(true);");
    driver.findElement({css:'span[about="v-wf:executor"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click 'executor' field ");});
    driver.findElement({css:'veda-control[class="VCexecutor fulltext dropdown create properties-editor"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click 'VCexecutor' field ");});

    var find = '4 Администратор4';
    driver.findElement({css:'veda-control[class="VCexecutor fulltext dropdown create properties-editor"] input[id="fulltext"]'}).sendKeys(find)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot find attribute 'VCexecutor'");});
    var executor = '4 Администратор4 : Аналитик';
    // Проверяем что запрашивамый объект появился в выпадающем списке
    driver.wait
    (
        function () {
            return driver.findElements({css:'veda-control[class="VCexecutor fulltext dropdown create properties-editor"] span[class="tt-dropdown-menu"] div[class="tt-dataset-dataset"] p'}).then(function (suggestions) {
                return webdriver.promise.filter(suggestions, function(suggestion) {
                    return suggestion.getText().then(function(txt){
                        return txt.toLowerCase() === executor.toLowerCase();
                    });
                }).then(function(x) { return x.length>0; });
            });
        },
        basic.FAST_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Cannot find '4 Администратор4 : Аналитик' from dropdown");});

    driver.findElements({css:'veda-control[class="VCexecutor fulltext dropdown create properties-editor"] span[class="tt-dropdown-menu"] div[class="tt-dataset-dataset"] p'}).then(function (suggestions) {
        webdriver.promise.filter(suggestions, function(suggestion) {
            return suggestion.getText().then(function(txt){
                if (executor === undefined) {
                    return txt.toLowerCase() == find.toLowerCase();
                } else {
                    return txt.toLowerCase() == executor.toLowerCase();
                }
            });
        }).then(function(x) { x[0].click();});
    }).thenCatch(function (e) {basic.errorHandler(e, "Cannot click on '4 Администратор4 :  Аналитик' from dropdown");});


    new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-io-condition-input .ep'}), driver.findElement({css:'.state-task'})).perform();
    new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-task .ep'}), driver.findElement({css:'.state-io-condition-output'})).perform();
    driver.findElement({css:'#workflow-save-button'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click save net");});

    startForm.createStartForm(driver, timeStamp, 'Ожидает отправки');
    driver.findElement({css:'.workflow-canvas-wrapper'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
    driver.findElement({css:'.state-io-condition-input[colored-to="red"]'})
        .thenCatch(function (e) {basic.errorHandler(e, "Seems 'input' button is not located/red");});
    driver.sleep(basic.FAST_OPERATION);
    driver.findElement({css:'.state-task[colored-to="red"]'})
        .thenCatch(function (e) {basic.errorHandler(e, "Seems 'state-task' button is not located/red");});
    driver.sleep(basic.FAST_OPERATION);
    driver.findElement({css:'.state-io-condition-output'})
        .thenCatch(function (e) {basic.errorHandler(e, "Seems 'output' button is not located/not colored");});
    driver.sleep(basic.FAST_OPERATION);
    
    driver.quit();
});


