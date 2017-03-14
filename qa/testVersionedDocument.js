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

function updateVersion(driver, task, label, valueToSearch, valueToChoose) {
    if(task == 'edit') {
        driver.executeScript("document.querySelector('#edit').scrollIntoView(true);");
        basic.execute(driver, 'click', 'button[id="edit"]', "Cannot click on `Edit` button");
    }
    driver.executeScript("document.querySelector('div[property=\"rdfs:label\"]').scrollIntoView(true);");
    basic.execute(driver, 'clear', 'veda-control[property="rdfs:label"] div[class="input-group"] input[type="text"]', "Cannot fill 'rdfs:label' field");
    basic.execute(driver, 'sendKeys', 'veda-control[property="rdfs:label"] div[class="input-group"] input[type="text"]', "Cannot fill 'rdfs:label' field", label);
    driver.executeScript("document.querySelector('strong[about=\"v-s:responsible\"]').scrollIntoView(true);");
    basic.execute(driver, 'clear', 'div[rel="v-s:responsible"] + veda-control input[id="fulltext"]', "Cannot find attribute `v-s:responsible`");
    basic.chooseFromDropdown(driver, 'v-s:responsible', valueToSearch, valueToChoose);
    driver.executeScript("document.querySelector('#save').scrollIntoView(true);");
    basic.execute(driver, 'click', 'button[id="save"]', "Cannot click on `Save` button");
}

/**
 * Проверка правильного порядка версий
 * @param driver
 * @param version - название версии
 * @param responsible - отвественный
 * @param shortlabel - краткое название
*/

function checkVersion(driver, version, responsible) {
    var i = 0;
    driver.sleep(basic.FAST_OPERATION);
    for(var j = 0; j < version.length; j++) {
        driver.sleep(basic.FAST_OPERATION).then(function() {
        var a;
            driver.findElement({css: 'div[rel="v-s:previousVersion"]'}).getText().then(function (result) {
                a = result;
                if (a != ("МероприятиеВерсия: " + version[i])) {
                    console.trace("Seems wrong version, expected: " + version[i]);
                    process.exit(1);
                } else {
                    driver.executeScript("document.querySelector('strong[about=\"v-s:previousVersion\"]').scrollIntoView(true);");
                    basic.execute(driver, 'click', 'div[rel="v-s:previousVersion"] span[typeof="v-s:Action v-s:Version"]', "Cannot click on 'previousVersion'");
                    driver.sleep(basic.FAST_OPERATION);
                    driver.executeScript("document.querySelector('strong[about=\"v-s:responsible\"]').scrollIntoView(true);");
                    var b = responsible[i];
                    driver.findElement({css: 'div[rel="v-s:responsible"]'}).getText().then(function (result) {
                        assert.equal("Назначение: " + b, result);
                    }).thenCatch(function (e) {basic.errorHandler(e, "Seems wrong responsible, expected" + b);})
                    i++;
                }
            }).thenCatch(function (e) {basic.errorHandler(e, "Invalid element");}); 
        });
    };
}

/**
 * 1.Open page -> login (as karpovrt);
 * 2.Open create Action document form -> updateVersion(Create Action1);
 * 3.Update Action1(Action2) -> Update Action1(Action3) -> Check versions -> Update Action2(Action4) -> Check versions;
 * 4.Quit;
 *
 * 1.Открывем страницу -> Входим в систему под karpovrt;
 * 2.Открываем форму создания Мероприятия -> Создаем мероприятие;
 * 3.Обновляем Мероприятие -> Обновляем Мероприятие -> Проверяем правильность версий -> Обновляем версию мероприятие
 * -> Проверяем правильность версий;
 * 4.Выход;
*/

basic.getDrivers().forEach(function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    basic.openCreateDocumentForm(driver, 'Мероприятие', 'v-s:Action');
    updateVersion(driver, 'new', timeStamp + 1, 'Администратор2', 'Администратор2 : Аналитик');

    updateVersion(driver, 'edit', timeStamp + 2, 'Администратор2', 'Администратор2 : Аналитик');
    updateVersion(driver, 'edit', timeStamp + 3, 'Администратор4', 'Администратор4 : Аналитик');
    checkVersion(driver, [timeStamp + 3, timeStamp + 2, timeStamp + 1], ['Администратор4 : Аналитик', 'Администратор2 : Аналитик', 'Администратор2 : Аналитик']);
    driver.executeScript("document.querySelector('strong[about=\"v-s:nextVersion\"]').scrollIntoView(true);");
    basic.execute(driver, 'click', 'div[rel="v-s:nextVersion"] span[typeof="v-s:Action v-s:Version"]', "Cannot click on 'nextVersion'");
    driver.executeScript("document.querySelector('strong[about=\"v-s:nextVersion\"]').scrollIntoView(true);");
    basic.execute(driver, 'click', 'div[rel="v-s:nextVersion"] span[typeof="v-s:Action v-s:Version"]', "Cannot click on 'nextVersion'");
    updateVersion(driver, 'edit', timeStamp + 4, 'Администратор2', 'Администратор2 : Аналитик');
    driver.sleep(basic.FAST_OPERATION);
    driver.executeScript("document.querySelector('strong[about=\"v-s:nextVersion\"]').scrollIntoView(true);");
    basic.execute(driver, 'click', 'div[rel="v-s:nextVersion"] span[typeof="v-s:Action"]', "Cannot click on 'nextVersion'");
    checkVersion(driver, [timeStamp + 4, timeStamp + 2, timeStamp + 1], ['Администратор2 : Аналитик', 'Администратор2 : Аналитик', 'Администратор2 : Аналитик']);
    driver.quit();
});
