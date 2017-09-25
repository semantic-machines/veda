var assert = require('assert'),
    basic = require('./basic.js'),
    complexRoute = require('./complexRoute.js'),
    createNet = require('./createNet.js'),
    startForm = require('./startForm.js'),
    timeStamp = ''+Math.round(+new Date()/1000),
    webdriver = require('selenium-webdriver');

/**
 * Нажатие на кнопку
 * @param driver
 * @param button - кнопка
 * @param doctype - тип документа
 * @param phase - текущая фаза теста
*/

function clickButton(driver, button, doctype, phase) {
    driver.executeScript("document.querySelector('button[id="+button+"]').scrollIntoView(true);")
        .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Cannot scroll to " + button + " button");});
    driver.wait
    (
        webdriver.until.elementIsEnabled(driver.findElement({css:'[typeof="'+doctype+'"] button[id="'+ button +'"]'})),
        basic.SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Cannot find " + button + " button");});
    basic.execute(driver, 'click', '[typeof="'+doctype+'"] button[id="'+ button +'"]', "****** PHASE#" + phase + " : ERROR = Cannot click on "  + button +  " button");
}

function put(driver, type, text, phase) {
    basic.execute(driver, 'sendKeys', 'div[property="' + type + '"]+veda-control[data-type="multilingualText"] textarea[class="form-control"]',
        "****** PHASE#" + phase + " : ERROR = Cannot fill "+ type +" field", text);
}

/**
 * 0.Open page -> login(as karpovrt);
 * 1.Create Rule1 -> Save Rule1 -> Create Transformation1 -> Save Tranformation1;
 * 2.Create Net -> Create Task with executor, Rule1 and Transformation -> Connect net -> Save net -> Check net is working;
 * 3.Logout -> login(as bychinat) -> Check messages(1);
 *
 * 0.Открываем страницу -> Входим в систему под karpovrt;
 * 1.Создаем Правило1 -> Сохраняем Правило1 -> Создаем трансформацию1 -> Сохраняем трансформацию1;
 * 2.Создаем сеть -> Создаем задачу с исполнителем, правилом1 и трансформацией1 -> Соединяем сеть -> Сохраняем сеть ->
 * -> Проверяем, что сеть работает;
 * 3.Выходим из системы -> Входим в систему под bychinat -> Проверяем сообщения(1);
*/
basic.getDrivers().forEach (function (drv) {
    //PHASE#0: Login
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 0);

    //PHASE#1: Create rule + transformation
    basic.openCreateDocumentForm(driver, 'Правило', 'v-wf:Rule');
    driver.executeScript("document.querySelector('div[property=\"rdfs:label\"]').scrollIntoView(true);")
        .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#1 : ERROR = Cannot scroll to 'rdfs:label' field");});
    basic.execute(driver, 'sendKeys', 'div[property="rdfs:label"]+veda-control[data-type="multilingualString"] input[type="text"]',
        "****** PHASE#1 : ERROR = Cannot fill 'rdfs:label' field", timeStamp);
    driver.executeScript("document.querySelector('div[property=\"v-wf:segregateElement\"]').scrollIntoView(true);")
        .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#1 : ERROR = Cannot scroll to 'segregateElement' field");});
    put(driver, 'v-wf:segregateElement', "contentName('@')", 1);
    driver.executeScript("document.querySelector('div[property=\"v-wf:aggregate\"]').scrollIntoView(true);")
        .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#1 : ERROR = Cannot scroll to 'aggregate' field");});
    put(driver, 'v-wf:aggregate', "putUri ('rdf:type', 'v-wf:DecisionForm');", 1);
    put(driver, 'v-wf:aggregate', "putUri ('rdf:type', 's-wf:UserTaskForm');", 1);
    put(driver, 'v-wf:aggregate', "putString ('rdfs:label', 'задание');", 1);
    put(driver, 'v-wf:aggregate', "putBoolean ('v-wf:isCompleted', false);", 1);
    put(driver, 'v-wf:aggregate', "if ( getUri(get_properties_chain(executor, [{$get: 'rdf:type'}], [executor])) === 'v-s:Appointment' ) {      putUri ('v-wf:to', getUri(get_properties_chain(executor, [{$get: 'v-s:employee'}], [executor])));    } else {      putExecutor ('v-wf:to');    }", 1);
    put(driver, 'v-wf:aggregate', "putWorkOrder ('v-wf:onWorkOrder');", 1);
    put(driver, 'v-wf:aggregate', "putUri ('v-wf:possibleDecisionClass', 'v-wf:DecisionAchieved');", 1);
    clickButton(driver, "save", "v-wf:Rule", 1);
    //driver.sleep(basic.FAST_OPERATION);
    basic.openCreateDocumentForm(driver, 'Трансформация', 'v-wf:Transform', 1);
    driver.executeScript("document.querySelector('div[property=\"rdfs:label\"]').scrollIntoView(true);")
        .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#1 : ERROR = Cannot scroll to 'rdfs:label' field");});
    basic.execute(driver, 'sendKeys', 'div[property="rdfs:label"]+veda-control[data-type="multilingualString"] input[type="text"]',
        "****** PHASE#1 : ERROR = Cannot fill 'rdfs:label' field", timeStamp + 1);
    driver.executeScript("document.querySelector('strong[about=\"v-wf:transformRule\"]').scrollIntoView(true);")
        .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#1 : ERROR = Cannot scroll to 'transformRule' field");});
    basic.chooseFromDropdown(driver, 'v-wf:transformRule', timeStamp, timeStamp, 1);
    clickButton(driver, "save", "v-wf:Transform", 1);

    //PHASE#2: Create net
    createNet.startNet(driver, timeStamp, 2);
    basic.execute(driver, 'click', '.create-task', "****** PHASE#2 : ERROR = Cannot click on 'create-task' button");
    basic.execute(driver, 'click', '.state-task', "****** PHASE#2 : ERROR = Cannot click on 'state-task' button");
    driver.executeScript("document.querySelector('span[about=\"v-wf:startDecisionTransform\"]').scrollIntoView(true);")
        .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#2 : ERROR = Cannot scroll to 'startDecisionTransform' field");});
    basic.execute(driver, 'click', 'span[about="v-wf:startDecisionTransform"]', "****** PHASE#2 : ERROR = Cannot click on 'v-wf:startDecisionTransform' field ");
    basic.execute(driver, 'click', 'veda-control[class="VCstartDecisionTransform fulltext dropdown create properties-editor"]',
        "****** PHASE#2 : ERROR = Cannot click on 'VCstartDecisionTransform' field ");
    createNet.chooseFromDropdown(driver, 'VCstartDecisionTransform', timeStamp + 1, timeStamp + 1, 2);
    driver.executeScript("document.querySelector('span[about=\"v-wf:executor\"]').scrollIntoView(true);")
        .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#2 : ERROR = Cannot scroll to 'executor' field");});
    basic.execute(driver, 'click', 'span[about="v-wf:executor"]', "****** PHASE#2 : ERROR = Cannot click on 'v-wf:executor' field ");
    basic.execute(driver, 'click', 'veda-control[class="VCexecutor fulltext dropdown create properties-editor"]',
        "****** PHASE#2 : ERROR = Cannot click on 'VCexecutor' field ");
    createNet.chooseFromDropdown(driver, 'VCexecutor', 'Администратор4', 'Администратор4 : Аналитик', 2);
    createNet.connectNet(driver, 'true');
    createNet.saveNet(driver, 2);
    createNet.checkNet(driver, timeStamp, 'red', 'red', '-', 2);

    //PHASE#3: Check message
    basic.logout(driver, 3);
    basic.login(driver, 'bychinat', '123', '4', 'Администратор4', 3);
    driver.findElement({css:'li[about="v-ft:Inbox"] span[id=counter]'}).getText().then(function (result) {
        assert.equal(1, result.length);
    }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#3 : ERROR = Invalid `message` elements count (inbox task counter)");});
    basic.logout(driver, 3);
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4', 3);
    driver.quit();
});
