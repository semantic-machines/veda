var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    assert = require('assert'),
    timeStamp = ''+Math.round(+new Date()/1000),
    now = new Date();

basic.getDrivers().forEach (function (drv) {
	var driver = basic.getDriver(drv);
	
	basic.openPage(driver, drv);
	basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');
	
	basic.openCreateDocumentForm(driver, 'Персона', 'v-s:Person');
	
	// Документ нельзя создать или отправить пока не заполнены обязательные поля
	driver.findElement({css:'div[typeof="v-s:Person"] > div.panel > div.panel-footer > button#save'}).isEnabled().then(function (flag) {
		assert(!flag);
	}).thenCatch(function (e) {basic.errorHandler(e, "Save button must be inactive")});
	
	// Удаляем раскрытый appointment
	driver.executeScript("document.querySelector('[rel=\"v-s:hasAppointment\"] button.button-delete').scrollIntoView(true);");
	driver.findElement({css:'[rel="v-s:hasAppointment"] button.button-delete'}).click().thenCatch(function (e) {basic.errorHandler(e, "Cannot delete appointment")});
	
	
	// Заполняем обязательные поля
	driver.findElement({css:'div[id="object"] [property="rdfs:label"] + veda-control input'}).sendKeys("Вася Пупкин "+timeStamp).thenCatch(function (e) {basic.errorHandler(e, "Cannot fill rdfs:label for preson")});
	driver.findElement({css:'[property="v-s:lastName"] + veda-control input'}).sendKeys("Пупкин").thenCatch(function (e) {basic.errorHandler(e, "Cannot fill v-s:lastName for preson")});
	driver.findElement({css:'[property="v-s:firstName"] + veda-control input'}).sendKeys("Вася").thenCatch(function (e) {basic.errorHandler(e, "Cannot fill v-s:firstName for preson")});
	driver.findElement({css:'[property="v-s:middleName"] + veda-control input'}).sendKeys(timeStamp).thenCatch(function (e) {basic.errorHandler(e, "Cannot fill v-s:middleName for preson")});
	driver.findElement({css:'[property="v-s:birthday"] + veda-control input'}).sendKeys(
			now.getFullYear() + '-' + ('0' + (now.getMonth() + 1)).slice(-2) + '-' + ('0' + now.getDate()).slice(-2))
			.thenCatch(function (e) {basic.errorHandler(e, "Cannot fill v-s:birthday for preson")});
	
	basic.chooseFromDropdown(driver, 'v-s:hasAccount', 'karpovrt', 'karpovrt');
	
	driver.executeScript("document.querySelector('[rel=\"v-s:hasAppointment\"] + veda-control input').scrollIntoView(true);");
	
	basic.chooseFromDropdown(driver, 'v-s:hasAppointment', 'Роман Карпов', 'Роман Карпов : Аналитик');
//	driver.findElement({css:'[property="v-s:lastName"] + veda-control input'}).click();

	driver.executeScript("$('div[typeof=\"v-s:Person\"] > div.panel > div.panel-footer > button#save')[0].scrollIntoView(true);");

	// Документ становится возможно сохранить
	driver.wait
	(
	  webdriver.until.elementIsEnabled(driver.findElement({css:'div[typeof="v-s:Person"] > div.panel > div.panel-footer > button#save'})),
	  basic.FAST_OPERATION
	).thenCatch(function (e) {basic.errorHandler(e, "Cannot find save button")});
	
	// Нажимаем сохранить
	driver.findElement({css:'div[typeof="v-s:Person"] > div.panel > div.panel-footer > button#save'}).click()
	      .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on save button")});
	
	// Проверяем что сохранение успешно
	// Переходим на страницу просмотра документа
	driver.findElement({css:'div[id="object"] > [typeof="v-s:Person"]'}).getAttribute('resource').then(function (individualId) {
		basic.openPage(driver, drv, '#/'+individualId);	
	}).thenCatch(function (e) {basic.errorHandler(e, "Seems person is not saved")});
	
	// Смотрим что в нём содержится введённый ранее текст
	driver.findElement({css:'div[property="v-s:middleName"] span[class="value-holder"]'}).getText().then(function (txt) {
		assert(txt == timeStamp);
	}).thenCatch(function (e) {basic.errorHandler(e, "Seems that person is not saved properly")});

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
