var assert = require('assert'),
    basic = require('./basic.js'),
    complexRoute = require('./complexRoute.js');

/**
 * 0.Open page -> login(as kaprovrt);
 * 1.Open create Complex route test template 2 document form -> Send task to coordinator1(bychinat) -> Logout;
 * 2.Login(as bychinat) -> Check task numbers(as bychinat) -> Accept task(as bychinat) -> Logout;
 * 3.Login(as kaprovrt) -> Search s-wf:ComplexRouteStartForm -> Open our Complex route test template 2 -> Send task again;
 * 4.Do p.2 again;
 *
 * 0.Открываем страницу -> Входим в систему под karpovrt;
 * 1.Открываем форму создания Тестовый шаблон комплексного маршурута 2 -> Отправляем задачу Согласующему1 -> Выходим из системы;
 * 2.Входим в систему под bychinat -> Проверяем количество задач -> Подтверждаем задачу -> Выходим из системы;
 * 3.Входим в систему под karpovrt -> Ищем Стартовую форму комплексного маршрута -> Открываем наш тестовый шаблон комплексного маршрута 2 ->
 * -> Посылаем задачу заново;
 * 4.Делаем п.2 снова;
*/

basic.getDrivers().forEach (function (drv) {
    //PHASE#0: Login
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 0);

    //PHASE#1: ComplexRouteTest2
    basic.openCreateDocumentForm(driver, 'Тестовый шаблон комплексного маршурута 2', 's-wf:ComplexRouteTest2', 1);
    driver.executeScript("document.querySelector('#send').scrollIntoView(true)");
    basic.execute(driver, 'click', 'button[id="send"]', "****** PHASE#1 : ERROR = Cannot click on 'Send' button");
    driver.sleep(basic.FAST_OPERATION);
    driver.executeScript("document.querySelector('#save_and_start_process').scrollIntoView(true)");
    driver.sleep(basic.FAST_OPERATION);
    basic.execute(driver, 'click', 'button[id="save_and_start_process"]', "****** PHASE#1 : ERROR = Cannot click on 'save_and_start_process' button");
    driver.sleep(basic.FAST_OPERATION);
    basic.logout(driver, 1);

    //PHASE#2: Check&Accept
    complexRoute.checkTask(driver, '1', 'bychinat', '123', '4', 'Администратор4', 2);
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_finish'] ,['red'], 0, 0);
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4', 2);
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_finish'] ,['red'], 1, 0);

    //PHASE#3: Repeat
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 3);
    basic.openFulltextSearchDocumentForm(driver, 'Стартовая форма сети Комплексный маршрут', 's-wf:ComplexRouteStartForm', 3);
    basic.execute(driver, 'click', 'button[id="submit"]', "****** PHASE#3 : ERROR = Cannot click on 'submit' button");
    driver.sleep(basic.FAST_OPERATION);
    basic.execute(driver, 'click', 'a[typeof="s-wf:ComplexRouteTest2"]', "****** PHASE#3 : ERROR = Cannot click on document id");
    basic.execute(driver, 'click', 'button[id="send"]', "****** PHASE#3 : ERROR = Cannot click on 'Send' button");
    driver.sleep(basic.FAST_OPERATION);
    driver.executeScript("document.querySelector('#save_and_start_process').scrollIntoView(true)");
    basic.execute(driver, 'click', 'button[id="save_and_start_process"]', "****** PHASE#3 : ERROR = Cannot click on 'save_and_start_process' button");
    driver.sleep(basic.FAST_OPERATION);
    basic.logout(driver, 3);

    //PHASE#4: Check&Accept
    complexRoute.checkTask(driver, '1', 'bychinat', '123', '4', 'Администратор4', 4);
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_finish'] ,['red'], 0, 1);
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4', 4);
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_finish'] ,['red'], 1, 1);
    driver.quit();
});