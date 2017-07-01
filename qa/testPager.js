var webdriver = require('selenium-webdriver'),
	basic = require('./basic.js');

/**
 * Проверка pager
 * @param driver
 * @param phase - текущая фаза теста
*/
function testPager(driver, phase) {
	driver.executeScript("document.querySelector('ul[id=\"pager\"]').scrollIntoView(true);");
	driver.findElements({css:'#pager > li:nth-child(2) > a'}).then(function (result) {
		if (result.length > 0) {
			basic.execute(driver, 'click', '#pager > li:nth-child(2) > a', "****** PHASE#" + phase + " : ERROR = Cannot click on 2 page");
			driver.sleep(basic.EXTRA_SLOW_OPERATION);
			driver.executeScript("document.querySelector('ul[id=\"pager\"]').scrollIntoView(true);");
			basic.execute(driver, 'click', '#pager > li:nth-child(1) > a', "****** PHASE#" + phase + " : ERROR = Cannot click on 1 page");
			driver.sleep(basic.SLOW_OPERATION);
		}
	}).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Seems something is wrong");});
}

/**
 * 0.Open page -> login(as karpovrt);
 * 1.Open Search menu -> Search request -> Test pager;
 * 2.Open Find menu -> Search request -> Test pager;
 *
 * 0.Открываем страницу -> Входим в систему под karpovrt;
 * 1.Открываем меню Найти -> Посылаем запрос -> Проверяем pager;
 * 2.Открываем меню Поиска -> Посылаем запрос -> Проверяем pager;
*/

basic.getDrivers().forEach(function(drv) {
	//PHASE#0: Login
	var driver = basic.getDriver(drv);
	basic.openPage(driver, drv);
	basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 0);

	//PHASE#1: Search menu
	basic.menu(driver, 'Search', 1);
	basic.execute(driver, 'sendKeys', '#q', "****** PHASE#1 : ERROR = Cannot fill input field",
		"'v-s:created' == [2013-06-30T12:13:11, 2017-01-30T12:13:11]");
	basic.execute(driver, 'click', 'button[id="search-submit"]', "****** PHASE#1 : ERROR = Cannot click on 'search-submit' button");
	driver.sleep(basic.EXTRA_SLOW_OPERATION);
	testPager(driver, 1);

    //PHASE#2: Find menu
	basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
	basic.execute(driver, 'click', 'button[id="submit"]', "****** PHASE#2 : ERROR = Cannot click on 'submit' button");
	driver.sleep(basic.EXTRA_SLOW_OPERATION);
	testPager(driver, 2);
	// TO DO: testPager in messages; OPTIMIZE;
	driver.quit();
});
