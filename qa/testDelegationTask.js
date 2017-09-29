var basic = require('./basic.js'),
    complexRoute = require('./complexRoute.js'),
    delegationRequest = require('./delegationRequest.js'),
    person = require('./person.js'),
    timeStamp = ''+Math.round(+new Date()/1000),
    webdriver = require('selenium-webdriver');
    

/**
 * 0.Open page -> login(as karpovrt)
 * 1.Create task for karpovrt -> Logout
 * 2.Check number of tasks as khvostiat(0) -> Check number of tasks as karpovrt(1);
 * 3.Create delegation request as karpovrt(for khvostiat);
 * 4.Accept task as khvostiat -> Check number of tasks as khvostiat(0) -> Check number of tasks as karpovrt(0);
 *
 * 0.Открываем страницу -> Входим в систему под karpovrt;
 * 1.Создаем задачу для karpovrt -> Выходим из системы;
 * 2.Проверяем, что нет задачи у khvostiat -> Проверяем, что у karpovt она есть;
 * 3.Создаем делегирование под karpovrt для bychiant;
 * 4.Отвечаем на появившуюся задачу у khvostiat -> Проверяем, что нет задачи у khvostiat -> Проверяем, что нет задачи у karpovrt;
 */

basic.getDrivers().forEach(function (drv) {
    //PHASE#0: Login
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 0);

    //PHASE#1: Create person
    basic.openCreateDocumentForm(driver, 'Тестовый шаблон комплексного маршурута', 's-wf:ComplexRouteTest', 1);
    basic.execute(driver, "click", 'span[about="v-s:SendTask"]', "****** PHASE#1 > Create task : ERROR = Cannot click on SendTask button");
    basic.execute(driver, "click", 'div[typeof="s-wf:ComplexRouteTest"] ul[id="standard-tasks"]');
    basic.chooseFromDropdown(driver, 'v-s:responsible', 'Администратор2', 'Администратор2 : Коммерческий директор', 1);
    basic.execute(driver, "sendKeys", 'veda-control[property="rdfs:comment"] textarea[class="form-control"]',
        "****** PHASE#1 > Create task : ERROR = Cannot fill Comment field", timeStamp);
    driver.sleep(basic.FAST_OPERATION * 2);
    basic.execute(driver, "click", 'div[class="modal-dialog modal-lg"] button[id="send"]', "****** PHASE#1 > Create task : ERROR = Cannot click on Send button");
    basic.logout(driver, 1);


    //PHASE#2: Check person
    complexRoute.checkTask(driver, '1', 'karpovrt', '123', '2', 'Администратор2', 2);
    complexRoute.checkTask(driver, '0', 'khvostiat', '123', '1', 'Администратор1', 2);

    //PHASE#3: Delegation request
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 3);
    delegationRequest.createRequestDelegation(driver, 'Администратор1', 'Администратор1 : Аналитик', "td:CommercialDirector", 3);
    driver.sleep(basic.FAST_OPERATION);
    basic.execute(driver, 'click', 'a[href="#/v-l:Welcome"]', "****** PHASE#3 : ERROR = Cannot click on 'Welcome' button");
    basic.logout(driver, 3);

    //PHASE#4: Check person
    basic.login(driver, 'khvostiat', '123', '1', 'Администратор1', 4);
    basic.menu(driver, 'Inbox', 4);
    driver.sleep(basic.SLOW_OPERATION);
    driver.findElement({css:'span[about="td:CommercialDirector"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#4 : ERROR = Cannot click on 'td:CommercialDirector' actor")});
    driver.sleep(basic.SLOW_OPERATION);
    driver.wait(basic.findUp(driver, 'a[property="rdfs:label"]', 3, "****** PHASE#4 : ERROR = Cannot find 'rdfs:label'"), basic.FAST_OPERATION).then(
        function(result){basic.clickUp(result, "****** PHASE#4 : ERROR = Cannot click on message");});
    basic.execute(driver, 'click', 'div[class="radio decision"] input[value="0"]', "****** PHASE#4 : ERROR = Cannot click on '0' decision");
    driver.sleep(basic.FAST_OPERATION);
    driver.executeScript("document.querySelector('#send').scrollIntoView(true)")
        .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#4 : ERROR = Cannot scroll to save button");});
    basic.execute(driver, 'click', 'button[id="send"]', "****** PHASE#4 : ERROR = Cannot click on 'Ok' button");
    basic.execute(driver, 'click', 'a[href="#/v-l:Welcome"]', "****** PHASE#4 : ERROR = Cannot click on 'Welcome' button");
    basic.logout(driver, 4);
    complexRoute.checkTask(driver, '0', 'khvostiat', '123', '1', 'Администратор1', 4);
    complexRoute.checkTask(driver, '0', 'karpovrt', '123', '2', 'Администратор2', 4);
    driver.quit();
});
