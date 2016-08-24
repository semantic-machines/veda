var webdriver = require('selenium-webdriver'),
	basic = require('./basic.js');

function search(driver, selector){
	driver.findElement({id:"menu"}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button");});
	driver.wait
	(
		webdriver.until.elementIsVisible(driver.findElement({css:'li[id="menu"] li[resource="v-l:Search"]'})),
		basic.FAST_OPERATION
	).thenCatch(function (e) {basic.errorHandler(e, "Seems there is no 'search' button inside menu");});
	driver.findElement({css:'li[id="menu"] li[resource="v-l:Search"]'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'search' button");});
	driver.findElement({css:'#q'}).sendKeys(selector)
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot fill input field");});
	driver.findElement({id:'search-submit'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'search-submit' button");});
	driver.sleep(basic.EXTRA_SLOW_OPERATION);
}

function testPager(driver) {
	driver.executeScript("document.querySelector('ul[id=\"pager\"]').scrollIntoView(true);");
	driver.findElement({css:'#pager > li:nth-child(2) > a'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 2 page");});
	driver.sleep(basic.EXTRA_SLOW_OPERATION	);
	driver.executeScript("document.querySelector('ul[id=\"pager\"]').scrollIntoView(true);");
	driver.findElement({css:'#pager > li:nth-child(1) > a'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 1 page");});
	driver.sleep(basic.EXTRA_SLOW_OPERATION);
}

basic.getDrivers().forEach(function(drv) {
	var driver = basic.getDriver(drv);
	basic.openPage(driver, drv);
	basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');
	search(driver, "'v-s:created' == [2013-06-30T12:13:11, 2017-01-30T12:13:11]");
	testPager(driver);

	// TO DO: testPager in messages

	driver.quit();
});