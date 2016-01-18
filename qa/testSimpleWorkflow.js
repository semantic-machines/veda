var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    assert = require('assert'),
    timeStamp = ''+Math.round(+new Date()/1000),
    now = new Date();

basic.getDrivers().forEach (function (drv) {
	var driver = basic.getDriver(drv);
	
	basic.openPage(driver, drv);
	basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');
	
	basic.openCreateDocumentForm(driver, 'Сеть', 'v-wf:Net');
	
	driver.sleep(basic.FAST_OPERATION);
	
	// Create new simple net
	driver.findElement({css:'.workflow-canvas-wrapper'}).click().thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas")});	
	driver.sleep(basic.FAST_OPERATION);
	driver.findElement({css:'#props-col [about="rdfs:label"]'}).click().thenCatch(function (e) {basic.errorHandler(e, "Cannot click on route rdfs:label")});
	

	driver.findElement({css:'#VClabel input'}).sendKeys(timeStamp)
			.thenCatch(function (e) {basic.errorHandler(e, "Cannot fill rdfl:label in net properties")});
	
	driver.findElement({css:'.create-task'}).click().thenCatch(function (e) {basic.errorHandler(e, "Cannot click add task")});	

	new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-io-condition-input .ep'}), driver.findElement({css:'.state-task'})).perform();
	
	new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-task .ep'}), driver.findElement({css:'.state-io-condition-output'})).perform();
	
	driver.findElement({css:'#workflow-save-button'}).click().thenCatch(function (e) {basic.errorHandler(e, "Cannot click save net")});
	
	driver.sleep(basic.FAST_OPERATION);
	
	// Create new start form	
	basic.openCreateDocumentForm(driver, 'Стартовая форма', 'v-wf:StartForm');
});
