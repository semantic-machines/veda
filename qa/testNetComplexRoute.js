var console = require('console');
var basic = require('./basic.js'),
    complexRoute = require('./complexRoute.js'),
    assert = require('assert');
/**
 * 1.Open page -> login(as karpovrt);
 * 2.Open create Complex route test template document form -> Start route -> Logout;
 * Coordination1:
 * 3.Accept task(as kaprovrt) -> Accept task(as bychinat);
 * Coordination2;
 * 4.Accept task(as kaprovrt) -> Decline task(as bychinat) -> Go to coordination2(as karpovrt) -> Accept task(as bychinat) ->
 * -> Accept task(as karpovrt);
 * review, instruction, examination -> instruction2
 * 5.Check task(as bychinat) -> Check task(as kaprovrt) -> Accept task(as bychinat) -> Check task(as karpovrt) ->
 * -> Accept task(as bychinat) -> Check task(as kaprovrt) -> Accept task(as bychinat);
 * Redirect -> not for me -> finalize -> rejected -> finalize -> achieved 
 * 6.Check task(as kaprovrt) -> Accept task(as kaprovrt) -> Accept task(as bychinat) -> Accept task(as kaprovrt) -> 
 * -> Accept task(as bychinat) -> Accept task(as kaprovrt) -> Accept task(as bychinat);
 * Controller
 * 7.Accept task(as kaprovrt) -> Accept task(as bychinat) -> Accept task(as bychinat) -> Accept task(as bychinat);
 * 8.Quit;
 *
 * 1.Открываем страницу -> Вход в систему под karpovrt;
 * 2.Открываем форму создания Тестовый шаблон комплексного маршурута -> Запускаем маршрут -> Выходим из системы;
 * Согласование1
 * 3.Отвечаем на задачу(под karpovrt) -> Отвечаем на задачу(под bychinat);
 * Согласование2
 * 4.Отвечаем на задачу(под karpovrt) -> Отклоняем задачу(под bychinat) -> Переходим к согласованию2(под karpovrt) ->
 * -> Отвечаем на задачу(под bychinat) -> Отвечаем на задачу(под kaprovrt);
 * Рассмотрение, Поручение, Ознакомление -> Поручение 2;
 * 5.Проверяем задачи(под bychinat) -> Проверяем задачи(под karpovrt) -> Отвечаем на задачу(под bychinat) -> 
 * -> Проверяем задачи(под karpovrt) -> Отвечаем на задачу(под bychinat) -> Проверяем задачи(под karpovrt) ->
 * -> Отвечаем на задачу(под bychinat) ;
 * Перенаправить -> Не мне -> Доработать -> Не выполнено -> Доработать -> Выполнено
 * 6.Проверяем задачи(под karpovrt) -> Отвечаем на задачу(под karpovrt) -> Отвечаем на задачу(под bychinat) ->
 * -> Отвечаем на задачу(под karpovrt) -> Отвечаем на задачу(под bychinat) ->
 * -> Отвечаем на задачу(под karpovrt) -> Отвечаем на задачу(под bychinat);
 * Контролирующий
 * 7.Отвечаем на задачу(под karpovrt) -> Отвечаем на задачу(под bychinat) ->
 * -> Отвечаем на задачу(под karpovrt) -> Отвечаем на задачу(под bychinat);
 * 8.Выход
*/

basic.getDrivers().forEach (function (drv) {
    var driver = basic.getDriver(drv);

    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    basic.openCreateDocumentForm(driver, 'Тестовый шаблон комплексного маршурута', 's-wf:ComplexRouteTest');
    driver.executeScript("document.querySelector('#send').scrollIntoView(true)");
    basic.execute(driver, 'click', 'button[id="send"]', "Cannot click on 'Send' button");
    driver.sleep(basic.SLOW_OPERATION);
    driver.executeScript("document.querySelector('#save_and_start_process').scrollIntoView(true)");
    driver.sleep(basic.FAST_OPERATION);
    basic.execute(driver, 'click', 'button[id="save_and_start_process"]',
        "Cannot click on 'save_and_start_process' button");
    driver.sleep(basic.FAST_OPERATION);
    basic.logout(driver);

    //coordination1

    complexRoute.acceptTask(driver, '0', '-', '-', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_c1', 's-wf:cr_c2'], ['green', 'red'], 1, 2);

    //coordination2

    complexRoute.acceptTask(driver, '0', '-', '-', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '1', '+', '-', 'bychinat', '123', '4', 'Администратор4');
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_c2', 's-wf:cr_rework'], ['green', 'red'], 1, 2);
    complexRoute.acceptTask(driver, '1', '-', '-', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');
    complexRoute.acceptTask(driver, '0', '-', '-', 'karpovrt', '123', '2', 'Администратор2');
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_review', 's-wf:cr_instruction', 's-wf:cr_examination'],
    //    ['red', 'red', 'red'], 1, 2);

    //review, instruction, examination -> instruction2
    complexRoute.checkTask(driver, '3', 'bychinat', '123', '4', 'Администратор4');
    complexRoute.checkTask(driver, '0', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '0', '-', '-',  'bychinat', '123', '4', 'Администратор4');
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_review', 's-wf:cr_instruction', 's-wf:cr_examination', 's-wf:cr_instruction2'],
    //    ['red', 'red', 'green', 'red'], 1);
    complexRoute.checkTask(driver, '0', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');
    // complexRoute.checkRouteStatus(driver, ['s-wf:cr_review', 's-wf:cr_instruction', 's-wf:cr_examination', 's-wf:cr_instruction2'],
    //     ['green', 'red', 'green', 'red'], 1);
    complexRoute.checkTask(driver, '0', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_review', 's-wf:cr_instruction', 's-wf:cr_examination', 's-wf:cr_instruction2'],
    //    ['green', 'green', 'green', 'red'], 1, 2);

    //
    complexRoute.checkTask(driver, '1', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '3', '+', '+', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '4', '+', '-', 'bychinat', '123', '4', 'Администратор4');
    complexRoute.acceptTask(driver, '1', '+', '-', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '2', '+', '-', 'bychinat', '123', '4', 'Администратор4');
    complexRoute.acceptTask(driver, '1', '+', '-', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_finish'] ,['red'], 0, 2);

    //controller
    complexRoute.acceptTask(driver, '2', '+', '+', 'karpovrt', '123', '2', 'Администратор2');
    complexRoute.acceptTask(driver, '1', '+', '-', 'bychinat', '123', '4', 'Администратор4');
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4');

    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_instruction2', 's-wf:cr_finish'], ['red' , 'red'], 1, 2);

    driver.quit();
});

