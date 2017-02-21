var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    timeStamp = ''+Math.round(+new Date()/1000),
    assert = require('assert');

/**
 * Обновление данных мероприятия на новые
 * @param driver
 * @param label - новое название
 * @param valueToSearch | Новый
 * @param valueToChoose | Ответственный
 * @param shortLabel - новое короткое название
*/

function updateVersion(driver, label, valueToSearch, valueToChoose, shortLabel) {
    driver.executeScript("document.querySelector('#edit').scrollIntoView(true);");
    driver.findElement({id:'edit'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Edit` button");});
    driver.executeScript("document.querySelector('div[property=\"rdfs:label\"]').scrollIntoView(true);");
    driver.findElement({css:'veda-control[property="rdfs:label"] div[class="input-group"] input[type="text"]'}).clear()
    driver.findElement({css:'veda-control[property="rdfs:label"] div[class="input-group"] input[type="text"]'}).sendKeys(label)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'rdfs:label' field");});
    driver.executeScript("document.querySelector('strong[about=\"v-s:responsible\"]').scrollIntoView(true);");
    driver.findElement({css:'div[rel="v-s:responsible"] + veda-control input[id="fulltext"]'}).clear()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot find attribute `v-s:responsible`")});
    basic.chooseFromDropdown(driver, 'v-s:responsible', valueToSearch, valueToChoose);
    driver.executeScript("document.querySelector('strong[about=\"v-s:shortLabel\"]').scrollIntoView(true);");
    driver.findElement({css:'veda-control[property="v-s:shortLabel"] div[class="input-group"] textarea[class="form-control"]'}).clear();
    driver.findElement({css:'veda-control[property="v-s:shortLabel"] div[class="input-group"] textarea[class="form-control"]'}).sendKeys(shortLabel)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'v-s:shortLabel' field");});
    driver.executeScript("document.querySelector('#save').scrollIntoView(true);");
    driver.findElement({id:'save'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Save` button");});
}

/**
 * Проверка правильного порядка версий
 * @param driver
 * @param version - название версии
 * @param responsible - отвественный
 * @param shortlabel - краткое название
*/

function checkVersion(driver, version, responsible, shortLabel) {
    var i = 0;
    driver.sleep(basic.FAST_OPERATION);
    for(var j = 0; j < version.length; j++) {
        driver.sleep(basic.FAST_OPERATION).then(function() {
        var a;
            driver.findElement({css: 'div[rel="v-s:previousVersion"]'}).getText().then(function (result) {
                a = result;
                if (a != ("МероприятиеВерсия: " + version[i])) {
                    console.trace("Seems wrong version, expected: " + version[j]);
                    process.exit(1);
                } else {
                    driver.executeScript("document.querySelector('strong[about=\"v-s:previousVersion\"]').scrollIntoView(true);");
                    driver.findElement({css: 'div[rel="v-s:previousVersion"] span[typeof="v-s:Action v-s:Version"]'}).click()
                        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'previousVersion'");});
                    driver.sleep(basic.FAST_OPERATION);
                    driver.executeScript("document.querySelector('strong[about=\"v-s:responsible\"]').scrollIntoView(true);");
                    var b = responsible[i], c = shortLabel[i];
                    driver.findElement({css: 'div[rel="v-s:responsible"]'}).getText().then(function (result) {
                        assert.equal("Назначение: " + b, result);
                    }).thenCatch(function (e) {basic.errorHandler(e, "Invalid  element");});
                    driver.executeScript("document.querySelector('strong[about=\"v-s:shortLabel\"]').scrollIntoView(true);");
                    driver.findElement({css: 'div[property="v-s:shortLabel"]'}).getText().then(function (result) {
                        assert.equal(c, result);
                    }).thenCatch(function (e) {basic.errorHandler(e, "Invalid  journal elements count");});
                    i++;
                }
            }).thenCatch(function (e) {basic.errorHandler(e, "Invalid element");}); 
        });
    };
}

/**
 * 1.Open page -> login (as karpovrt);
 * 2.Open create Action document form -> Fill label, responsible, shortLabel -> Save;
 * 3.Update Action1(Action2) -> Check versions -> Update Action2(Action3) -> Check versions;
 * 4.Quit;
 *
 * 1.Открывем страницу -> Входим в систему под karpovrt;
 * 2.Открываем форму создания Мероприятия -> Заполняем название, ответственного, короткое название -> Сохраняем;
 * 3.Обновляем Мероприятие -> Проверяем правильность версий -> Обновляем Мероприятие -> Проверяем правильность версий;
 * 4.Выход;
*/

basic.getDrivers().forEach(function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    basic.openCreateDocumentForm(driver, 'Мероприятие', 'v-s:Action');
    driver.executeScript("document.querySelector('div[property=\"rdfs:label\"]').scrollIntoView(true);");
    driver.findElement({css:'veda-control[property="rdfs:label"] div[class="input-group"] input[type="text"]'}).sendKeys('Action1')
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'rdfs:label' field");});
    driver.executeScript("document.querySelector('strong[about=\"v-s:responsible\"]').scrollIntoView(true);");
    basic.chooseFromDropdown(driver, 'v-s:responsible', "Администратор2", "Администратор2 : Аналитик");
    driver.executeScript("document.querySelector('strong[about=\"v-s:shortLabel\"]').scrollIntoView(true);");
    driver.findElement({css:'veda-control[property="v-s:shortLabel"] div[class="input-group"] textarea[class="form-control"]'}).sendKeys('v1')
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'v-s:shortLabel' field");});
    driver.executeScript("document.querySelector('#save').scrollIntoView(true);");
    driver.findElement({id:'save'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Save` button");});


    updateVersion(driver, 'Action2', 'Администратор2', 'Администратор2 : Аналитик', 'v2');
    checkVersion(driver, ['Action2', 'Action1'],
        ['Администратор2 : Аналитик', 'Администратор2 : Аналитик'], ['v2', 'v1']);
    //updateVersion(driver, 'Action3', 'Администратор4', 'Администратор4 : Аналитик', 'v3');
    //checkVersion(driver, ['Action3', 'Action2', 'Action1'],
    //    ['Администратор4 : Аналитик', 'Администратор2 : Аналитик', 'Администратор2 : Аналитик'], ['v3', 'v2', 'v1']);

    driver.quit();
});
