var basic = require('./basic.js'),
    createNet = require('./createNet.js'),
    timeStamp = ''+Math.round(+new Date()/1000);
/**
 * 0.Open page -> login(as karpovrt);
 * 1.Create net -> Create task -> Connect input, task and output -> Save net;
 * 2.Check net is working;
 *
 * 0.Открываем страницу -> Входим в систему под karpovrt;
 * 1.Создаем сеть -> Создаем задачу -> Соединяем вход, задачу и выход -> Сохраняем сеть;
 * 2.Проверяем, что сеть работает;
*/

basic.getDrivers().forEach (function (drv) {
    //PHASE#0: Login
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 0);

    //PHASE#1: Create net
    createNet.startNet(driver, timeStamp, 1);
    createNet.createTask(driver, '', 'false', 1);
    createNet.connectNet(driver, 'true');
    createNet.saveNet(driver, 1);

    //PHASE#2: Check net
    createNet.checkNet(driver, timeStamp, 'red', 'green', 'red', 2);
    driver.quit();
});

