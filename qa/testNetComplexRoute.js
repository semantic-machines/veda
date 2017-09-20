var console = require('console');
var basic = require('./basic.js'),
    complexRoute = require('./complexRoute.js'),
    assert = require('assert');

/**
 * 0.Open page -> login(as karpovrt);
 * 1.Open create Complex route test template document form -> Start route -> Logout;
 * Coordination1:
 * 2.Accept task(as kaprovrt) -> Accept task(as bychinat);
 * Coordination2;
 * 3.Accept task(as kaprovrt) -> Decline task(as bychinat) -> Go to coordination2(as karpovrt) -> Accept task(as bychinat) ->
 * -> Accept task(as karpovrt);
 * review, instruction, examination -> instruction2
 * 4.Check task(as bychinat) -> Check task(as kaprovrt) -> Accept task(as bychinat) -> Check task(as karpovrt) ->
 * -> Accept task(as bychinat) -> Check task(as kaprovrt) -> Accept task(as bychinat);
 * Redirect -> not for me -> finalize -> rejected -> finalize -> achieved
 * 5.Check task(as kaprovrt) -> Accept task(as kaprovrt) -> Accept task(as bychinat) -> Accept task(as kaprovrt) ->
 * -> Accept task(as bychinat) -> Accept task(as kaprovrt) -> Accept task(as bychinat);
 * Controller
 * 6.Accept task(as kaprovrt) -> Accept task(as bychinat) -> Accept task(as bychinat) -> Accept task(as bychinat);
 *
 * 0.Открываем страницу -> Вход в систему под karpovrt;
 * 1.Открываем форму создания Тестовый шаблон комплексного маршурута -> Запускаем маршрут -> Выходим из системы;
 * Согласование1
 * 2.Отвечаем на задачу(под karpovrt) -> Отвечаем на задачу(под bychinat);
 * Согласование2
 * 3.Отвечаем на задачу(под karpovrt) -> Отклоняем задачу(под bychinat) -> Переходим к согласованию2(под karpovrt) ->
 * -> Отвечаем на задачу(под bychinat) -> Отвечаем на задачу(под kaprovrt);
 * Рассмотрение, Поручение, Ознакомление -> Поручение 2;
 * 4.Проверяем задачи(под bychinat) -> Проверяем задачи(под karpovrt) -> Отвечаем на задачу(под bychinat) ->
 * -> Проверяем задачи(под karpovrt) -> Отвечаем на задачу(под bychinat) -> Проверяем задачи(под karpovrt) ->
 * -> Отвечаем на задачу(под bychinat) ;
 * Перенаправить -> Не мне -> Доработать -> Не выполнено -> Доработать -> Выполнено
 * 5.Проверяем задачи(под karpovrt) -> Отвечаем на задачу(под karpovrt) -> Отвечаем на задачу(под bychinat) ->
 * -> Отвечаем на задачу(под karpovrt) -> Отвечаем на задачу(под bychinat) ->
 * -> Отвечаем на задачу(под karpovrt) -> Отвечаем на задачу(под bychinat);
 * Контролирующий
 * 6.Отвечаем на задачу(под karpovrt) -> Отвечаем на задачу(под bychinat) ->
 * -> Отвечаем на задачу(под karpovrt) -> Отвечаем на задачу(под bychinat);
*/

basic.getDrivers().forEach (function (drv) {
    //PHASE#0: Login
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 0);

    //PHASE#1: Start route
    basic.openCreateDocumentForm(driver, 'Тестовый шаблон комплексного маршурута', 's-wf:ComplexRouteTest', 1);
    driver.executeScript("document.querySelector('#send').scrollIntoView(true)")    
        .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#1 : ERROR = Cannot scroll to send button");});
    basic.execute(driver, 'click', 'button[id="send"]', "****** PHASE#1 : ERROR = Cannot click on 'Send' button");
    driver.sleep(basic.SLOW_OPERATION * 2);
    driver.executeScript("document.querySelector('#save_and_start_process').scrollIntoView(true)")
        .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#1 : ERROR = Cannot scroll to save_and_start_process button");});
    driver.sleep(basic.FAST_OPERATION * 2);
    basic.execute(driver, 'click', 'button[id="save_and_start_process"]',
        "****** PHASE#1 : ERROR = Cannot click on 'save_and_start_process' button");
    driver.sleep(basic.FAST_OPERATION * 2);
    basic.logout(driver, 1);

    //PHASE#2: Coordination1
    complexRoute.acceptTask(driver, '0', '-', '-', 'karpovrt', '123', '2', 'Администратор2', 2);
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4', 2);
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_c1', 's-wf:cr_c2'], ['green', 'red'], 1, 2);

    //PHASE#3: Coordination2
    complexRoute.acceptTask(driver, '0', '-', '-', 'karpovrt', '123', '2', 'Администратор2', 3);
    complexRoute.acceptTask(driver, '1', '+', '-', 'bychinat', '123', '4', 'Администратор4', 3);
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_c2', 's-wf:cr_rework'], ['green', 'red'], 1, 2);
    complexRoute.acceptTask(driver, '1', '-', '-', 'karpovrt', '123', '2', 'Администратор2', 3);
    complexRoute.acceptTask(driver, '0', '-', '-', 'bychinat', '123', '4', 'Администратор4', 3);
    complexRoute.acceptTask(driver, '0', '-', '-', 'karpovrt', '123', '2', 'Администратор2', 3);
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_review', 's-wf:cr_instruction', 's-wf:cr_examination'],
    //    ['red', 'red', 'red'], 1, 2);

    //PHASE#4: Review, instruction, examination -> instruction2
    complexRoute.checkTask(driver, '2', 'bychinat', '123', '4', 'Администратор4', 4);
    complexRoute.checkTask(driver, '0', 'karpovrt', '123', '2', 'Администратор2', 4);
    complexRoute.acceptTask(driver, '0', '+', '-',  'bychinat', '123', '4', 'Администратор4', 4);
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_review', 's-wf:cr_instruction', 's-wf:cr_examination', 's-wf:cr_instruction2'],
    //    ['red', 'red', 'green', 'red'], 1);
    complexRoute.checkTask(driver, '0', 'karpovrt', '123', '2', 'Администратор2', 4);
    complexRoute.acceptTask(driver, '0', '+', '-', 'bychinat', '123', '4', 'Администратор4', 4);
    // complexRoute.checkRouteStatus(driver, ['s-wf:cr_review', 's-wf:cr_instruction', 's-wf:cr_examination', 's-wf:cr_instruction2'],
    //     ['green', 'red', 'green', 'red'], 1);
    // complexRoute.checkTask(driver, '0', 'karpovrt', '123', '2', 'Администратор2', 'examination');
    // complexRoute.acceptTask(driver, '0', '+', '-', 'bychinat', '123', '4', 'Администратор4');
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_review', 's-wf:cr_instruction', 's-wf:cr_examination', 's-wf:cr_instruction2'],
    //    ['green', 'green', 'green', 'red'], 1, 2);

    //PHASE#5:
    // ? complexRoute.checkTask(driver, '1', 'karpovrt', '123', '2', 'Администратор2', '?');
    complexRoute.acceptTask(driver, '3', '+', '+', 'karpovrt', '123', '2', 'Администратор2', 5);
    complexRoute.acceptTask(driver, '4', '+', '-', 'bychinat', '123', '4', 'Администратор4', 5);
    complexRoute.acceptTask(driver, '1', '+', '-', 'karpovrt', '123', '2', 'Администратор2', 5);
    complexRoute.acceptTask(driver, '2', '+', '-', 'bychinat', '123', '4', 'Администратор4', 5);
    complexRoute.acceptTask(driver, '1', '+', '-', 'karpovrt', '123', '2', 'Администратор2', 5);
    complexRoute.acceptTask(driver, '0', '+', '-', 'bychinat', '123', '4', 'Администратор4', 5);
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_finish'] ,['red'], 0, 2);

    //PHASE#6: Simple route
    complexRoute.acceptTask(driver, '2', '+', '+', 'karpovrt', '123', '2', 'Администратор2', 6);
    complexRoute.acceptTask(driver, '1', '+', '-', 'bychinat', '123', '4', 'Администратор4', 6);
    complexRoute.acceptTask(driver, '0', '+', '-', 'bychinat', '123', '4', 'Администратор4', 6);
    complexRoute.acceptTask(driver, '0', '+', '-', 'bychinat', '123', '4', 'Администратор4', 6);
    //complexRoute.checkRouteStatus(driver, ['s-wf:cr_instruction2', 's-wf:cr_finish'], ['red' , 'red'], 1, 2);
    driver.quit();
});

