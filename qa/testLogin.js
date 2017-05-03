var basic = require('./basic.js');
var console = require('console');

/**
 * 1.Open Page -> Login(as karpovrt);
 * 2.Quit;
 *
 * 1.Открываем страницу -> Входим в систему под karpovrt;
 * 2.Выход
 */

basic.getDrivers().forEach (function (drv) {
	console.time("testLogin");

	var driver = basic.getDriver(drv);
	basic.openPage(driver, drv);
	basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

	driver.quit();	
	console.timeEnd("testLogin");
});
