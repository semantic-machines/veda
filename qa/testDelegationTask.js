var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    complexRoute = require('./complexRoute.js'),
    person = require('./person.js'),
    delegationRequest = require('./delegationRequest.js'),
    timeStamp = ''+Math.round(+new Date()/1000);
    

/**
 * 0.Open page -> login(as karpovrt)
 * 1.Create task for karpovrt -> Logout
 * 2.Login(as bychinat) -> Check number of tasks(0) -> Logout;
 * 3.Login(as kaprovrt) -> Create delegation request -> Logout;
 * 4.Login(as bychinat) -> Check number of tasks(1) -> Logout;
 *
 * 0.Открываем страницу -> Входим в систему под karpovrt;
 * 1.Создаем задачу для karpovrt -> Выходим из системы;
 * 2.Входим в систему под bychinat -> Проверяем, что нет задачи у bychinat -> Выходим из системы;
 * 3.Входим в систему под karpovrt -> Создаем делегирование -> Выходим из системы;
 * 4.Входим в систему под bychinat -> Проверяем, что после делегирования появилась задача у bychinat -> Выходим из системы;
 */

basic.getDrivers().forEach(function (drv) {
    //PHASE#0: Login
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 0);

    //PHASE#1: Create perso
    basic.openCreateDocumentForm(driver, 'Тестовый шаблон комплексного маршурута', 's-wf:ComplexRouteTest', 1);
    basic.execute(driver, "click", 'span[about="v-s:SendTask"]', "****** PHASE#1 > Create task : ERROR = Cannot click on SendTask button");
    basic.execute(driver, "click", 'div[typeof="s-wf:ComplexRouteTest"] ul[id="standard-tasks"]');
    basic.chooseFromDropdown(driver, 'v-s:hasAppointment', 'Администратор2', 'Администратор2 : Коммерческий директор', 1);
    basic.execute(driver, "sendKeys", 'veda-control[property="rdfs:comment"] textarea[class="form-control"]',
        "****** PHASE#1 > Create task : ERROR = Cannot fill Comment field", timeStamp);
    driver.sleep(basic.FAST_OPERATION * 2);
    basic.execute(driver, "click", 'div[class="modal-dialog modal-lg"] button[id="send"]', "****** PHASE#1 > Create task : ERROR = Cannot click on Send button");
    basic.logout(driver, 1);


    //PHASE#2: Check person
    complexRoute.checkTask(driver, '0', 'bychinat', '123', '4', 'Администратор4', 2);
    complexRoute.checkTask(driver, '1', 'karpovrt', '123', '2', 'Администратор2', 2);

    //PHASE#3: Delegation request
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 3);
    //delegationRequest.createRequestDelegation(driver, 'Администратор4', 'Администратор4 : Аналитик', "td:Analyst1", 3);
    delegationRequest.createRequestDelegation(driver, 'Администратор4', 'Администратор4 : Аналитик', "td:CommercialDirector", 3);
    driver.sleep(basic.FAST_OPERATION);
    basic.execute(driver, 'click', 'a[href="#/v-l:Welcome"]', "****** PHASE#3 : ERROR = Cannot click on 'Welcome' button");
    basic.logout(driver, 3);

    //PHASE#4: Check person
    //complexRoute.checkTask(driver, '1', 'bychinat', '123', '4', 'Администратор4', 4);
    basic.login(driver, 'bychinat', '123', '4', 'Администратор4', 4);
    basic.menu(driver, 'Inbox', 4);
    driver.sleep(basic.SLOW_OPERATION);
    driver.findElement({css:'span[about="td:CommercialDirector"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'td:CommercialDirector' actor")});
    driver.wait(basic.findUp(driver, 'a[property="rdfs:label"]', 3, "****** PHASE#4 : ERROR = Cannot find 'rdfs:label'"), basic.FAST_OPERATION).then(
        function(result){basic.clickUp(result);});
    basic.execute(driver, 'click', 'div[class="radio decision"] input[value="0"]', "****** PHASE#4 : ERROR = Cannot click on '0' decision");
    driver.sleep(basic.FAST_OPERATION);
    driver.executeScript("document.querySelector('#send').scrollIntoView(true)");
    basic.execute(driver, 'click', 'button[id="send"]', "****** PHASE#4 : ERROR = Cannot click on 'Ok' button");
    basic.execute(driver, 'click', 'a[href="#/v-l:Welcome"]', "Cannot click on 'Welcome' button");
    basic.logout(driver, 4);
    //complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4', 4);
    complexRoute.checkTask(driver, '0', 'bychinat', '123', '4', 'Администратор4', 4);
    complexRoute.checkTask(driver, '0', 'karpovrt', '123', '2', 'Администратор2', 4);
    driver.quit();
});
