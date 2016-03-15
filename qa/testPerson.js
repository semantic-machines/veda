var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    person = require('./person.js'),
    assert = require('assert'),
    timeStamp = ''+Math.round(+new Date()/1000);

basic.getDrivers().forEach (function (drv) {
	var driver = basic.getDriver(drv);
	
	basic.openPage(driver, drv);
	basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');
	
	person.createPerson(driver, drv, timeStamp);
	
	// Открываем поисковый бланк
	basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
	
	// Вводим текст запроса
	driver.findElement({css:'h4[about="v-fs:EnterQuery"]+div[class="form-group"] input'}).sendKeys(timeStamp)
		  .thenCatch(function (e) {basic.errorHandler(e, "Cannot input search request")});;
	
	// Нажимаем поиск и удостоверяемся что в результатах поиска появился созданный выше документ  
    driver.sleep(basic.EXTRA_SLOW_OPERATION);
	driver.wait
	(
	  function () {
		  driver.findElement({css:'h4[about="v-fs:EnterQuery"]+div[class="form-group"] button[id="submit"]'}).click();
		  driver.sleep(basic.FAST_OPERATION); // Иначе слишком часто щелкает поиск
		  return driver.findElement({css:'span[href="#params-ft"]+span[class="badge"]'}).getText().then(function (txt) {
			  return txt == '1';
		  });
	  },
	  basic.EXTRA_SLOW_OPERATION
	).thenCatch(function (e) {basic.errorHandler(e, "Cannot find person, after save operation")});
	
	driver.wait
	(  
	  webdriver.until.elementTextContains(driver.findElement({css:'div[id="search-results"] span[property="v-s:middleName"]'}),timeStamp),
	  basic.FAST_OPERATION
	).thenCatch(function (e) {basic.errorHandler(e, "Found person differs from saved person")});
	
	driver.quit();
});
