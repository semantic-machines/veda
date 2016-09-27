var basic = require('./basic.js');
    
basic.getDrivers().forEach (function (drv) {
	var driver = basic.getDriver(drv);
	basic.openPage(driver, drv);

	basic.login(driver, 'karpovrt', '123', '2', 'Optiflow');

	driver.quit();	
});
