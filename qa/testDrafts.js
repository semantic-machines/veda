var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    person = require('./person.js'),
    firstName = ''+Math.round(+new Date()/1000),
    assert = require('assert');

function check(driver, count) {
    driver.findElement({id:'menu'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button")});
    driver.wait
    (
        webdriver.until.elementIsVisible(driver.findElement({css:'li[id="menu"] li[resource="v-l:Drafts"]'})),
        basic.FAST_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Seems there is no `drafts` button inside menu")});
    driver.findElement({css:'li[id="menu"] li[resource="v-l:Drafts"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `drafts` button")});
    driver.sleep(basic.FAST_OPERATION);
    driver.findElements({css:'div[id="drafts"] span[typeof="v-s:Person"'}).then(function(elements_arr){
        if (elements_arr.length > 0) {
            driver.wait
            (
                webdriver.until.elementIsVisible(driver.findElement({css: 'div[id="drafts"] span[typeof="v-s:Person"]'})),
                basic.FAST_OPERATION
            ).thenCatch(function (e) {basic.errorHandler(e, "Seems there is no `drafts` elements")});
            if (count == "true") {
                driver.findElement({css: 'div[id="drafts"] span[typeof="v-s:Person"]'}).click()
                    .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on selected draft")})
            }
            if (count == "false") {
                console.trace("Expected number of drafts is 0, but get 1");
                process.exit(1);
            }
        }
        if (elements_arr.length == 0){
            if (count == "true") {
                console.trace("Expected number of drafts is 1, but get 0");
                process.exit(1);
            }
        }
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems there is no `drafts` field")});
}



basic.getDrivers().forEach(function(drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', 'Карпов', 'Роман');
    //Создаем Черновик
    basic.openCreateDocumentForm(driver, 'Персона', 'v-s:Person');
    driver.findElement({css:'div[typeof="v-s:Person"] > div.panel > div.panel-footer > button#save'}).isEnabled().then(function (flag) {
        assert(!flag);
    }).thenCatch(function (e) {basic.errorHandler(e, "Save button must be inactive")});
    var lastName = 'Draft';
    driver.findElement({css:'[property="v-s:lastName"] + veda-control input'}).sendKeys(lastName).thenCatch(function (e) {basic.errorHandler(e, "Cannot fill v-s:lastName for preson")});
    driver.findElement({css:'[property="v-s:firstName"] + veda-control input'}).sendKeys(firstName).thenCatch(function (e) {basic.errorHandler(e, "Cannot fill v-s:firstName for preson")});
    driver.executeScript("$('div[typeof=\"v-s:Person\"] > div.panel > div.panel-footer > button#draft')[0].scrollIntoView(true);");
    driver.wait
    (
        webdriver.until.elementIsEnabled(driver.findElement({css:'div[typeof="v-s:Person"] > div.panel > div.panel-footer > button#draft'})),
        basic.FAST_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Cannot find 'draft' button")});
    driver.findElement({css:'div[typeof="v-s:Person"] > div.panel > div.panel-footer > button#draft'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'draft' button")});

    driver.sleep(basic.FAST_OPERATION);
    driver.findElement({css:'div[property="v-s:firstName"] span[class="value-holder"]'}).getText().then(function (txt) {
        assert(txt == firstName);
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems that person is not saved properly/FN")});
    driver.findElement({css:'div[property="v-s:lastName"] span[class="value-holder"]'}).getText().then(function (txt) {
        assert(txt == lastName);
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems that person is not saved properly/LN")});

    //Проверям наличие его в наших черновиках
    check(driver, "true");
    //Досоздаем черновик
    driver.executeScript("$('div[typeof=\"v-s:Person\"] > div.panel > div.panel-footer > button#edit')[0].scrollIntoView(true);");
    driver.wait
    (
        webdriver.until.elementIsEnabled(driver.findElement({css:'div[typeof="v-s:Person"] > div.panel > div.panel-footer > button#edit'})),
        basic.FAST_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Cannot find 'edit' button")});
    driver.findElement({css:'div[typeof="v-s:Person"] > div.panel > div.panel-footer > button#edit'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'edit' button")});
    driver.findElement({css:'[property="v-s:middleName"] + veda-control input'}).sendKeys('Пупкин').thenCatch(function (e) {basic.errorHandler(e, "Cannot fill v-s:middleName for preson")});
    var now = new Date();
    driver.findElement({css:'[property="v-s:birthday"] + veda-control input'}).sendKeys(
        now.getFullYear() + '-' + ('0' + (now.getMonth() + 1)).slice(-2) + '-' + ('0' + now.getDate()).slice(-2))
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill v-s:birthday for person")});
    driver.findElement({css:'[property="v-s:middleName"] + veda-control input'}).click().thenCatch(function (e) {basic.errorHandler(e, "Cannot click middle name control for person")});
    //Сохраняем его как нормальный документ
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

    //Проверяем, что его нет в черновиках
    check(driver, "false");

    driver.quit();
})
