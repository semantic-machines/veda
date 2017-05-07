var webdriver = require('selenium-webdriver'),
	basic = require('./basic.js');

/**
 * Проверка pager
 * @param driver
*/
function testPager(driver) {
	driver.executeScript("document.querySelector('ul[id=\"pager\"]').scrollIntoView(true);");

	driver.findElements({css:'#pager > li:nth-child(2) > a'}).then(function (result) {
		if (result.length > 0) {
			basic.execute(driver, 'click', '#pager > li:nth-child(2) > a', "Cannot click on 2 page");
			driver.sleep(basic.EXTRA_SLOW_OPERATION);
			driver.executeScript("document.querySelector('ul[id=\"pager\"]').scrollIntoView(true);");
                        basic.execute(driver, 'click', '#pager > li:nth-child(1) > a', "Cannot click on 1 page");
			driver.sleep(basic.SLOW_OPERATION);
		}
	}).thenCatch(function (e) {basic.errorHandler(e, "Seems something is wrong");});
}

/**
 * 1.Open page -> login(as karpovrt);
 * 2.Open Search menu -> Search request -> Test pager;
 * 3.Open Find menu -> Search request -> Test pager;
 * 4.Quit;
 *
 * 1.Открываем страницу -> Входим в систему под karpovrt;
 * 2.Открываем меню поиска -> Посылаем запрос -> Проверяем pager;
 * 3.Открываем другое меню поиска -> Посылаем запрос -> Проверяем pager;
 * 4.Выход;
*/

basic.getDrivers().forEach(function(drv) {
	var driver = basic.getDriver(drv);
	basic.openPage(driver, drv);
	basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

	basic.menu(driver, 'Search');
	basic.execute(driver, 'sendKeys', '#q', "Cannot fill input field", "'v-s:created' == [2013-06-30T12:13:11, 2017-01-30T12:13:11]");
	basic.execute(driver, 'click', 'button[id="search-submit"]', "Cannot click on 'search-submit' button");
	driver.sleep(basic.EXTRA_SLOW_OPERATION);
	testPager(driver);

	basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
        basic.execute(driver, 'click', 'button[id="submit"]', "Cannot click on 'submit' button");
	driver.sleep(basic.EXTRA_SLOW_OPERATION);
	testPager(driver);

	// TO DO: testPager in messages; OPTIMIZE;

	driver.quit();
});
