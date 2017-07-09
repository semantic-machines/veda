var basic = require('./basic.js');

/**
 * 1.Open Page -> Login(as karpovrt);
 *
 * 1.Открываем страницу -> Входим в систему под karpovrt;
 */

basic.getDrivers().forEach (function (drv) {
    //PHASE#1: Login
	var driver = basic.getDriver(drv);
	basic.openPage(driver, drv);
	basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 1);
	driver.quit();	
});
