var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    person = require('./person.js'),
    timeStamp = ''+Math.round(+new Date()/1000);

/**
 * 1.Open Page -> Login(as karpovrt);
 * 2.Create Person -> Open fullText search document form -> Search our person -> Check result -> Check result is right
 * 3.Quit;
 *
 * 1.Открываем страницу -> Входим в систему под karpovrt;
 * 2.Создаем Персону -> Открываем форму Полнотекстового поиска -> Ищем созданную персону -> Проверяем, что поиск дал результат ->
 * -> Проверяем, что результат верный;
 * 3.Конец;
 */

basic.getDrivers().forEach (function (drv) {
	var driver = basic.getDriver(drv);
	basic.openPage(driver, drv);
	basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

	var now = new Date();
	person.createPerson(driver, drv, 'Пупкин', 'Вася', timeStamp, ('0' + now.getDate()).slice(-2) + '.' + ('0' + (now.getMonth() + 1)).slice(-2) + '.' + now.getFullYear());
	// Открываем поисковый бланк
	basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
	// Вводим текст запроса
	basic.execute(driver, 'sendKeys', 'h4[about="v-fs:EnterQuery"]+div[class="form-group"] input', "Cannot find input field", timeStamp);
	// Нажимаем поиск и удостоверяемся что в результатах поиска появился созданный выше документ
	driver.wait
	(
	  function () {
		  basic.execute(driver, 'click', 'h4[about="v-fs:EnterQuery"]+div[class="form-group"] button[id="submit"]', "Cannot click on submit button");
		  driver.sleep(basic.FAST_OPERATION); // Иначе слишком часто щелкает поиск
		  return driver.findElement({css:'span[href="#params-ft"]+span[class="badge"]'}).getText().then(function (txt) {
			  return txt == '1';
		  });
	  },
	  basic.EXTRA_SLOW_OPERATION
	).thenCatch(function (e) {basic.errorHandler(e, "Cannot find person, after save operation");});
	driver.wait
	(
	  webdriver.until.elementTextContains(driver.findElement({css:'div[id="search-results"] span[property="v-s:middleName"]'}),timeStamp),
	  basic.FAST_OPERATION
	).thenCatch(function (e) {basic.errorHandler(e, "Found person differs from saved person");});
	
	driver.quit();
});
