var basic = require('./basic.js'),
    createNet = require('./createNet'),
    timeStamp = ''+Math.round(+new Date()/1000);
/**
 * 0.Open page -> login(as karpovrt);
 * 1.Create net -> Connect input and output -> Save net;
 * 2.Check our net is working;
 *
 * 0.Открываем страницу -> Входим в систему под karpovrt;
 * 1.Создаем сеть -> Соединяем вход и выход -> Сохраняем сеть;
 * 2.Проверяем, что наша сеть работает;
*/

basic.getDrivers().forEach (function (drv) {
    //PHASE#0: Login
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 0);

    //PHASE#1: Create net
    createNet.startNet(driver, timeStamp, 1);
    createNet.connectNet(driver, 'false');
    createNet.saveNet(driver, 1);

    //PHASE#2: Check net
    createNet.checkNet(driver, timeStamp, 'red', '-', 'red', 2);
    driver.quit();
});
