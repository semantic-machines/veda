var basic = require('./basic.js'),
    complexRoute = require('./complexRoute.js'),
    timeStamp = ''+Math.round(+new Date()/1000);


/**
 * Отправка задачи
 * @param driver
 * @param valueToSearch - значение, которое будет введено в качестве получателя задачи
 * @param valueToChoose - значение, которое будет выбрано в качестве получателя задачи
 */

function sendTask(driver, valueToSearch, valueToChoose) {
    basic.openCreateDocumentForm(driver, 'Тестовый шаблон комплексного маршурута', 's-wf:ComplexRouteTest', 1);
    basic.execute(driver, "click", 'span[about="v-s:SendTask"]', "****** PHASE#1 > Create task : ERROR = Cannot click on SendTask button");
    basic.execute(driver, "click", 'div[typeof="s-wf:ComplexRouteTest"] ul[id="standard-tasks"]');
    basic.chooseFromDropdown(driver, 'v-s:responsible', valueToSearch, valueToChoose, 1);
    basic.execute(driver, "sendKeys", 'veda-control[property="rdfs:comment"] textarea[class="form-control"]',
        "****** PHASE#1 > Create task : ERROR = Cannot fill Comment field", timeStamp);
    driver.sleep(basic.FAST_OPERATION * 4);
    basic.execute(driver, "click", 'div[class="modal-dialog modal-lg"] button[id="send"]', "****** PHASE#1 > Create task : ERROR = Cannot click on Send button");
    basic.execute(driver, 'click', 'a[href="#/v-l:Welcome"]', "****** PHASE#1 > Create task : ERROR = Cannot click on 'Welcome' button");
}


/**
 * 0. Open Page -> login(as karpovrt);
 * 1. Create task(bychinat - executor) -> Send task;
 * 2. Check tasks(as karpovrt)(Inbox: 2, Outbox: 3, Completed: 0) -> Check tasks(as bychinat) (Inbox: 1, Outbox: 0, Completed: 0);
 * 3. Accept task(as bychinat) -> Check tasks(as bychinat)(Inbox: 0, Outbox: 0, Completed: 1) ->
 *    -> Check tasks(as kaprovrt) (Inbox: 2, Outbox: 2, Completed: 0) -> Accept task(as karpovrt) -> Check tasks(as bychinat)(Inbox: 0, Outbox: 0, Completed: 1) ->
 *    -> Check tasks(as kaprovrt) (Inbox: 1, Outbox: 1, Completed: 1) -> Accept task(as bychinat) -> Check tasks(as bychinat)(Inbox: 0, Outbox: 0, Completed: 1) ->
 *    -> Check tasks(as kaprovrt) (Inbox: 0, Outbox: 0, Completed: 2)
 *
 * 0. Открываем страницу -> Входим в систему под karpovrt;
 * 1. Создаем задачу(bychinat - исполнитель) -> Отправляем задачу;
 * 2. Проверяем количество задач(bychinat(1; 0; 0), karpovrt(2; 3; 0))
 * 3. Исполняем задачу(за bychinat) -> Проверяем количество задач(bychinat(0; 0; 1), karpovrt(2; 2; 0)) -> 
 *    -> Исполняем задачу(за karpovrt) -> Проверяем количество задач(bychinat(0; 0; 1), karpovrt(1; 1; 1)) ->
 *    -> Исполняем задачу(за karpovrt) -> Проверяем количество задач(bychinat(0; 0; 1), karpovrt(0; 0; 2));
 */


basic.getDrivers().forEach(function (drv) {
    //PHASE#0: Login
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 0);

    //PHASE#1: Create task
    sendTask(driver, 'Администратор4', 'Администратор4 : Аналитик');
    sendTask(driver, 'Администратор2', 'Администратор2 : Аналитик');    
    sendTask(driver, 'Администратор2', 'Администратор2 : Коммерческий директор');    
    basic.logout(driver, 1);

    //PHASE#2: Check tasks
    complexRoute.checkTasks(driver, 1, 0, 0, 'bychinat', '123', '4', 'Администратор4', 2);
    complexRoute.checkTasks(driver, 2, 3, 0, 'karpovrt', '123', '2', 'Администратор2', 2);

    //PHASE#3: Accept + Check
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4', 3);
    complexRoute.checkTasks(driver, 0, 0, 1, 'bychinat', '123', '4', 'Администратор4', 3);
    complexRoute.checkTasks(driver, 2, 2, 0, 'karpovrt', '123', '2', 'Администратор2', 3);
    complexRoute.acceptTask(driver, '0', '-', '-', 'karpovrt', '123', '2', 'Администратор2', 3);
    complexRoute.checkTasks(driver, 0, 0, 1, 'bychinat', '123', '4', 'Администратор4', 3);
    complexRoute.checkTasks(driver, 1, 1, 1, 'karpovrt', '123', '2', 'Администратор2', 3);
    complexRoute.acceptTask(driver, '0', '-', '-', 'karpovrt', '123', '2', 'Администратор2', 3);
    complexRoute.checkTasks(driver, 0, 0, 1, 'bychinat', '123', '4', 'Администратор4', 3);
    complexRoute.checkTasks(driver, 0, 0, 2, 'karpovrt', '123', '2', 'Администратор2', 3);
    
    driver.quit();
});