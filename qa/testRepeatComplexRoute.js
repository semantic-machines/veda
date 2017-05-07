var basic = require('./basic.js'),
    complexRoute = require('./complexRoute.js'),
    assert = require('assert');
/**
 * 1.Open page -> login(as kaprovrt);
 * 2.Open create Complex route test template 2 document form -> Send task to coordinator1 -> Logout;
 * 3.Login(as bychinat) -> Check task numbers(as bychinat) -> Accept task(as bychinat) -> Logout; 
 * 4.Login(as kaprovt) -> Search s-wf:ComplexrRouteStartForm -> Open our Complex route test template 2 -> Send task again;
 * 5.Do p.3 again;
 * 6.Quit;
 *
 * 1.Открываем страницу -> Входим в систему под karpovrt;
 * 2.Открываем форму создания Тестовый шаблон комплексного маршурута 2 -> Отправляем задачу Согласующему1 -> Выходим из системы;
 * 3.Входим в систему под bychinat -> Проверяем количество задач -> Подтверждаем задачу -> Выходим из системы;
 * 4.Входим в систему под karpovrt -> Ищем Стартовую форму комплексного маршрута -> Открываем наш тестовый шаблон комплексного маршрута 2 ->
 * -> Посылаем задачу заново;
 * 5.Делаем п.3 снова;
 * 6.Quit;
*/

basic.getDrivers().forEach (function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    basic.openCreateDocumentForm(driver, 'Тестовый шаблон комплексного маршурута 2', 's-wf:ComplexRouteTest2');
    driver.executeScript("document.querySelector('#send').scrollIntoView(true)");
    basic.execute(driver, 'click', 'button[id="send"]', "Cannot click on 'Send' button");
    driver.sleep(basic.FAST_OPERATION);
    driver.executeScript("document.querySelector('#save_and_start_process').scrollIntoView(true)");
    driver.sleep(basic.FAST_OPERATION);
    basic.execute(driver, 'click', 'button[id="save_and_start_process"]', "Cannot click on 'save_and_start_process' button");
    driver.sleep(basic.FAST_OPERATION);
    basic.logout(driver);

    complexRoute.checkTask(driver, '1', 'bychinat', '123', '4', 'Администратор4');
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_finish'] ,['red'], 0, 0);
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_finish'] ,['red'], 1, 0);

    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    basic.openFulltextSearchDocumentForm(driver, 'Стартовая форма сети Комплексный маршрут', 's-wf:ComplexRouteStartForm');
    basic.execute(driver, 'click', 'button[id="submit"]', "Cannot click on 'submit' button");
    driver.sleep(basic.FAST_OPERATION);
    basic.execute(driver, 'click', 'a[typeof="s-wf:ComplexRouteTest2"]', "Cannot click on document id");
    basic.execute(driver, 'click', 'button[id="send"]', "Cannot click on 'Send' button");
    driver.sleep(basic.FAST_OPERATION);
    driver.executeScript("document.querySelector('#save_and_start_process').scrollIntoView(true)");
    basic.execute(driver, 'click', 'button[id="save_and_start_process"]', "Cannot click on 'save_and_start_process' button");
    driver.sleep(basic.FAST_OPERATION);
    basic.logout(driver);

    complexRoute.checkTask(driver, '1', 'bychinat', '123', '4', 'Администратор4', '#1');
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_finish'] ,['red'], 0, 1);
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_finish'] ,['red'], 1, 1);

    driver.quit();
});