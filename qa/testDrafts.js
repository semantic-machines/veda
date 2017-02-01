var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    firstName = ''+Math.round(+new Date()/1000),
    assert = require('assert');

function check(driver, count) {
    driver.findElement({id:'menu'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button");});
    basic.isVisible(driver, 'li[id="menu"] li[resource="v-l:Drafts"]', basic.FAST_OPERATION);
    driver.findElement({css:'li[id="menu"] li[resource="v-l:Drafts"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `drafts` button");});
    driver.sleep(basic.FAST_OPERATION);
    driver.findElements({css:'div[id="drafts"] span[typeof="v-s:Person"]'}).then(function(elements_arr){
        if (elements_arr.length > 0) {
            if (count == "true") {
                driver.findElement({css: 'div[id="drafts"] span[typeof="v-s:Person"]'}).click()
                    .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on selected draft");});
            }
            if (count == "false") {
                console.trace("Expected number of drafts is 0, but get 1");
                process.exit(1);
            }
        }
        if (elements_arr.length === 0){
            if (count == "true") {
                console.trace("Expected number of drafts is 1, but get 0");
                process.exit(1);
            }
        }
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems there is no `drafts` field");});
}



basic.getDrivers().forEach(function(drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    basic.openCreateDocumentForm(driver, 'Персона', 'v-s:Person');
    driver.findElement({css:'div[typeof="v-s:Person"] > .action#save'}).isEnabled().then(function (flag) {
        assert(!flag);
    }).thenCatch(function (e) {basic.errorHandler(e, "Save button must be inactive");});
    var lastName = 'Draft';
    driver.findElement({css:'[property="v-s:lastName"] + veda-control input'}).sendKeys(lastName)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'v-s:lastName' for person");});
    driver.findElement({css:'[property="v-s:firstName"] + veda-control input'}).sendKeys(firstName)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'v-s:firstName' for person");});
    driver.executeScript("$('div[typeof=\"v-s:Person\"] > .action#draft')[0].scrollIntoView(true);");
    basic.isEnabled(driver, '#draft', basic.FAST_OPERATION);
    driver.findElement({css:'#draft'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'draft' button");});
    driver.findElement({css:'div[property="v-s:firstName"] span[class="value-holder"]'}).getText().then(function (txt) {
        assert(txt == firstName);
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems that person is not saved properly/FN");});
    driver.findElement({css:'div[property="v-s:lastName"] span[class="value-holder"]'}).getText().then(function (txt) {
        assert(txt == lastName);
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems that person is not saved properly/LN");});

    //Проверям наличие его в наших черновиках
    check(driver, "true");
    //Досоздаем черновик
    driver.executeScript("$('div[typeof=\"v-s:Person\"] > .action#edit')[0].scrollIntoView(true);");
    basic.isEnabled(driver, '#edit');
    driver.findElement({css:'#edit'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'edit' button");});
    driver.findElement({css:'[property="v-s:middleName"] + veda-control input'}).sendKeys('Пупкин')
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'v-s:middleName' for person");});
    var now = new Date();
    driver.findElement({css:'[property="v-s:birthday"] + veda-control input'}).sendKeys(
        now.getFullYear() + '-' + ('0' + (now.getMonth() + 1)).slice(-2) + '-' + ('0' + now.getDate()).slice(-2))
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'v-s:birthday' for person");});
    driver.findElement({css:'[property="v-s:lastName"] + veda-control input'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'last name control' for person");});
    //Сохраняем его как нормальный документ
    driver.executeScript("$('div[typeof=\"v-s:Person\"] > .action#save')[0].scrollIntoView(true);");
    basic.isEnabled(driver, '#edit', basic.FAST_OPERATION);
    driver.findElement({css:'#save'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'save' button");});
    check(driver, "false");

    driver.quit();
})
