var webdriver = require('selenium-webdriver'),
	basic = require('./basic.js');

function menu(driver, attr){
	driver.findElement({id:"menu"}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button");});
	driver.wait
	(
		webdriver.until.elementIsVisible(driver.findElement({css:'li[id="menu"] li[resource="'+ attr +'"]'})),
		basic.FAST_OPERATION
	).thenCatch(function (e) {basic.errorHandler(e, "Seems there is no 'search' button inside menu");});
	driver.findElement({css:'li[id="menu"] li[resource="'+ attr +'"]'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'search' button");});
}

function testPager(driver) {
	driver.executeScript("document.querySelector('ul[id=\"pager\"]').scrollIntoView(true);");

	driver.findElements({css:'#pager > li:nth-child(2) > a'}).then(function (result) {
		if (result.length > 0) {
			driver.findElement({css:'#pager > li:nth-child(2) > a'}).click()
				.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 2 page");});
			driver.sleep(basic.EXTRA_SLOW_OPERATION);
			driver.executeScript("document.querySelector('ul[id=\"pager\"]').scrollIntoView(true);");
			driver.findElement({css:'#pager > li:nth-child(1) > a'}).click()
				.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 1 page");});
			driver.sleep(basic.SLOW_OPERATION);
		}
	}).thenCatch(function (e) {basic.errorHandler(e, "Seems something is wrong");});
}

basic.getDrivers().forEach(function(drv) {
	var driver = basic.getDriver(drv);
	basic.openPage(driver, drv);
	basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
	menu(driver, 'v-l:Search');
	driver.findElement({css:'#q'}).sendKeys("'v-s:created' == [2013-06-30T12:13:11, 2017-01-30T12:13:11]")
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot fill input field");});
	driver.findElement({id:'search-submit'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'search-submit' button");});
	driver.sleep(basic.EXTRA_SLOW_OPERATION);
	testPager(driver);

	basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
	driver.findElement({id:'submit'}).click()
	 	.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'submit' button");});
	driver.sleep(basic.EXTRA_SLOW_OPERATION);
	testPager(driver);

	// TO DO: testPager in messages; OPTIMIZE;

	driver.quit();
});
