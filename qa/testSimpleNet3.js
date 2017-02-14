var basic = require('./basic.js'),
    createNet = require('./createNet.js'),
    timeStamp = ''+Math.round(+new Date()/1000);
/**
 * 1.Open page -> login(as karpovrt);
 * 2.Create net -> Create task with executor -> Connect input, task and output -> Save net;
 * 3.Check net is working;
 * 4.Quit;
 * 
 * 1.Открываем страницу -> Входим в систему под karpovrt;
 * 2.Создаем сеть -> Создаем задачу с исполнителем -> Соединяем входи, задачу и выход -> Сохраняем сеть;
 * 3.Проверяем, что сеть работает;
 * 4.Выход;
*/

basic.getDrivers().forEach (function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    createNet.startNet(driver, timeStamp);
    createNet.createTask(driver, '4 Администратор4', 'Администратор4 : Аналитик');
    createNet.connectNet(driver, 'true');
    createNet.saveNet(driver);
    createNet.checkNet(driver, timeStamp, 'red', 'red', '-');
    driver.quit();
});


