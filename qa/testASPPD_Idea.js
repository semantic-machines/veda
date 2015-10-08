var webdriver = require('selenium-webdriver'),
    connection = require('./connection.js'),
    driver = connection.driver,
    basic = require('./basic.js'),
    assert = require('assert'),
    timeStamp = ''+Math.round(+new Date()/1000);

basic.openPage(driver);
basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');

basic.openCreateDocumentForm(driver, 'Идея', 'mnd-s-asppd:Idea');

// Документ нельзя создать или отправить пока не заполнены обязательные поля
driver.findElement({id:'save'}).isEnabled().then(function (flag) {
	assert(!flag);
});

// Заполняем обязательные поля
driver.findElement({css:'[property="mnd-s:registrationNumber"] + veda-control input'}).sendKeys(timeStamp);
driver.findElement({css:'[property="mnd-s:registrationNumber"] + veda-control input'}).sendKeys(webdriver.Key.ENTER);

// Документ становится возможно сохранить
driver.wait
(
  webdriver.until.elementIsEnabled(driver.findElement({id:'save'})),
  basic.FAST_OPERATION
).then (null, function(err) {if (console.trace!==undefined) {console.trace(err); process.exit(1);}});

driver.executeScript("document.getElementById('save').scrollIntoView(true);");

// Нажимаем сохранить
driver.findElement({id:'save'}).click();

// Проверяем что сохранение успешно
// Переходим на страницу просмотра документа
var individualId = driver.findElement({css:'[typeof="mnd-s-asppd:Idea"]'}).getAttribute('resource').then(function (individualId) {
	basic.openPage(driver, '#/individual/'+individualId+'/#main');	
});

// Смотрим что в нём содержится введённый ранее текст
driver.findElement({css:'div[property="mnd-s:registrationNumber"] span[class="value-holder"]'}).getText().then(function (txt) {
	assert(txt == timeStamp);
});

// Открываем поисковый бланк
basic.openFulltextSearchDocumentForm(driver, 'Идея');

// Вводим текст запроса
driver.findElement({css:'h4[about="v-fs:EnterQuery"]+div[class="form-group"] input'}).sendKeys(timeStamp);

// Нажимаем поиск
driver.findElement({css:'h4[about="v-fs:EnterQuery"]+div[class="form-group"] button[id="submit"]'}).sendKeys(timeStamp);

driver.quit();