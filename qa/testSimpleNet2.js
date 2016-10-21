var basic = require('./basic.js'),
    createNet = require('./createNet.js'),
    timeStamp = ''+Math.round(+new Date()/1000);

basic.getDrivers().forEach (function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    createNet.startNet(driver, timeStamp);
    createNet.createTask(driver, '', 'false');
    createNet.connectNet(driver, 'true');
    createNet.saveNet(driver);
    createNet.checkNet(driver, timeStamp, 'red', 'green', 'red');
    driver.quit();
});

