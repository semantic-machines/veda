var basic = require('./basic.js'),
    until = require('selenium-webdriver').until,
    By = require('selenium-webdriver').By;

basic.getDrivers().forEach (function (drv) {
	var driver = basic.getDriver(drv);
	
	basic.openPage(driver, drv, 'tests');
	
	driver.wait
	(
	  new until.Condition('all API tests execution', function () 
	  {
	    return driver.findElements(By.css("#qunit-tests>li")).then(
	     function(elements)
	     {
	       return driver.findElements(By.css("#qunit-tests>.pass")).then(
	         function(elements2)
	         {
	           return (elements.length == elements2.length);
	         })
	     });
	  })
	  ,
	  90000
	).then
	(
	  null,
	  function(err)
	  {
	    console.trace(err);
	    process.exit(1);
	  }
	);
	driver.quit();

});