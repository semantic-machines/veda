var webdriver = require('selenium-webdriver'),
    connection = require('./connection.js'),
    driver = connection.driver,
    basic = require('./basic.js'),
    assert = require('assert'),
    timeStamp = ''+Math.round(+new Date()/1000),
    now = new Date();

basic.openPage(driver);
basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');

basic.openCreateDocumentForm(driver, 'Персона', 'v-s:Person');

// Документ нельзя создать или отправить пока не заполнены обязательные поля
driver.findElement({id:'save'}).isEnabled().then(function (flag) {
	assert(!flag);
});

// Заполняем обязательные поля
driver.findElement({css:'div[id="object"] [property="rdfs:label"] + veda-control input'}).sendKeys("Вася Пупкин "+timeStamp);
driver.findElement({css:'[property="v-s:lastName"] + veda-control input'}).sendKeys("Пупкин");
driver.findElement({css:'[property="v-s:firstName"] + veda-control input'}).sendKeys("Вася");
driver.findElement({css:'[property="v-s:middleName"] + veda-control input'}).sendKeys(timeStamp);
driver.findElement({css:'[property="v-s:birthday"] + veda-control input'}).sendKeys(
		now.getFullYear() + '-' + ('0' + (now.getMonth() + 1)).slice(-2) + '-' + ('0' + now.getDate()).slice(-2));

basic.chooseFromDropdown(driver, 'v-s:hasAccount', 'karpovrt', 'karpovrt');
basic.chooseFromDropdown(driver, 'v-s:hasAppointment', 'Роман Карпов', 'Роман Карпов : Аналитик');
driver.findElement({css:'[property="v-s:lastName"] + veda-control input'}).click();

// Документ становится возможно сохранить
driver.wait
(
  webdriver.until.elementIsEnabled(driver.findElement({id:'save'})),
  basic.FAST_OPERATION
);

driver.executeScript("document.getElementById('save').scrollIntoView(true);");

// Нажимаем сохранить
driver.findElement({id:'save'}).click();

// Проверяем что сохранение успешно
// Переходим на страницу просмотра документа
driver.findElement({css:'div[id="object"] > [typeof="v-s:Person"]'}).getAttribute('resource').then(function (individualId) {
	basic.openPage(driver, '#/individual/'+individualId+'/#main');	
});

// Смотрим что в нём содержится введённый ранее текст
driver.findElement({css:'div[property="v-s:middleName"] span[class="value-holder"]'}).getText().then(function (txt) {
	assert(txt == timeStamp);
});

// Открываем поисковый бланк
basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');

// Вводим текст запроса
driver.findElement({css:'h4[about="v-fs:EnterQuery"]+div[class="form-group"] input'}).sendKeys(timeStamp);

// Нажимаем поиск и удостоверяемся что в результатах поиска появился созданный выше документ  
driver.wait
(
  function () {
	  driver.findElement({css:'h4[about="v-fs:EnterQuery"]+div[class="form-group"] button[id="submit"]'}).click();
	  driver.sleep(1000); // Иначе слишком часто щелкает поиск
	  return driver.findElement({css:'span[href="#params-ft"]+span[class="badge"]'}).getText().then(function (txt) {
		  return txt == '1';
	  });
  },
  basic.EXTRA_SLOW_OPERATION
);

driver.wait
(  
  webdriver.until.elementTextContains(driver.findElement({css:'div[id="search-results"] span[property="v-s:middleName"]'}),timeStamp),
  basic.FAST_OPERATION
);
driver.quit();