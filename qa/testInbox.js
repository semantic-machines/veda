var webdriver = require('selenium-webdriver'),
    timeStamp = ''+Math.round(+new Date()/1000),
    complexRoute = require('./complexRoute.js');
    basic = require('./basic.js');


/**
 * 1. Open Page -> login(as karpovrt);
 * 2. Create task(bychinat - executor) -> Send task;
 * 3. Check tasks(as bychinat)(Inbox: 0, Outbox: 1, Completed: 0) -> Check tasks(as bychinat) (Inbox: 1, Outbox: 0, Completed: 0);
 * 4. Accept task(as bychinat) -> Check tasks(as bychinat)(Inbox: 0, Outbox: 0, Completed: 0) ->
 *    -> Check tasks(as bychinat) (Inbox: 0, Outbox: 0, Completed: 1);
 *
 * 1. Открываем страницу -> Входим в систему под karpovrt;
 * 2. Создаем задачу(bychinat - исполнитель) -> Отправляем задачу;
 * 3. Проверяем количество задач
 * 4.
 *
 */


basic.getThreeDrivers().forEach(function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    basic.openCreateDocumentForm(driver, 'Тестовый шаблон комплексного маршурута', 's-wf:ComplexRouteTest');
    basic.execute(driver, "click", 'span[about="v-s:SendTask"]', "Cannot click on SendTask button");
    basic.execute(driver, "click", 'div[typeof="s-wf:ComplexRouteTest"] ul[id="standard-tasks"]');
    basic.execute(driver, "sendKeys", 'veda-control[rel="v-s:hasAppointment"] input[id="fulltext"]',
        "Cannot fill Appointment field", "Aдминистратор4 : Аналитик");
    //Dropdown
    basic.execute(driver, "sendKeys", 'veda-control[property="rdfs:comment"] textarea[class="form-control"]',
        "Cannot fill Comment field", timeStamp);
    driver.sleep(basic.FAST_OPERATION);
    basic.execute(driver, "click", 'button[id="Send"]', "Cannot click on Send button");

    complexRoute.checkTasks(driver, 1, 0, 0, 'bychinat', '123', '4', 'Администратор4');
    complexRoute.checkTasks(driver, 0, 1, 0, 'karpovrt', '123', '2', 'Администратор2');

    complexRoute.acceptTask(driver, 0, '-', '-', 'bychinat', '123', '4', 'Администратор4');
    complexRoute.checkTasks(driver, 0, 0, 1, 'bychinat', '123', '4', 'Администратор4');
    complexRoute.checkTasks(driver, 0, 0, 0, 'karpovrt', '123', '2', 'Администратор2');


    driver.quit();
});