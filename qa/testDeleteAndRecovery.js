var webdriver = require('selenium-webdriver'),
    timeStamp = ''+Math.round(+new Date()/1000),
    basic = require('./basic.js');

function check(driver, count) {
    basic.openFulltextSearchDocumentForm(driver, 'Стартовая форма', 'v-wf:StartForm');
    driver.findElement({css:'h4[about="v-fs:EnterQuery"]+div[class="form-group"] input'}).clear();
    driver.findElement({css:'h4[about="v-fs:EnterQuery"]+div[class="form-group"] input'}).sendKeys(timeStamp)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot input search request");});
    driver.findElement({css:'button[id="submit"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'submit' button");});
    driver.wait
    (
        function () {
            driver.findElement({css:'div[id="fulltext-search"] a[id="refresh"]'}).click()
                .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'refresh' button");});
            driver.sleep(basic.FAST_OPERATION);
            return driver.findElement({css:'span[href="#params-ft"]+span[class="badge"]'}).getText().then(function (txt) {
                return txt == count;
            });
        },
        basic.EXTRA_SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Number of elements is wrong, expected: " + count);});
}

function clickButton(driver, button) {
    driver.executeScript("document.querySelector('button[id="+button+"]').scrollIntoView(true);");
    driver.wait
    (
        webdriver.until.elementIsEnabled(driver.findElement({css:'button[id="'+ button +'"]'})),
        basic.SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Cannot find " + button + " button");});
    driver.findElement({css:'button[id="'+ button +'"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on "  + button +  " button");});
}

basic.getDrivers().forEach(function(drv){
    var driver = basic.getDriver(drv);
    
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    basic.openCreateDocumentForm(driver, 'Стартовая форма', 'v-wf:StartForm');
    driver.executeScript("document.querySelector('strong[about=\"rdfs:label\"]').scrollIntoView(true);");
    driver.findElement({css:'veda-control[data-type="multilingualString"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'rdfs:label' field");});
    driver.findElement({css:'veda-control[data-type="multilingualString"] input[type="text"]'}).sendKeys(timeStamp)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'rdfs:label' field");});
    clickButton(driver, "save");
    check(driver, 1);

    driver.findElement({css:'span[typeof="v-wf:StartForm"]'}).click()
        .thenCatch(function (e) {bssic.errorHandler(e, "Cannot click on 'StartForm' button");});
    clickButton(driver, "delete");
    driver.switchTo().alert().accept();
    check(driver, 0);

    driver.findElement({id:"menu"}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button");});
    basic.isVisible(driver, 'li[id="menu"] li[resource="v-l:Search"]', basic.FAST_OPERATION);
    driver.findElement({css:'li[id="menu"] li[resource="v-l:Search"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `search` button");});
    driver.findElement({css:'#q'}).sendKeys("'rdfs:label' == '"+ timeStamp + "' && 'v-s:deleted' == 'true'")
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill input field");});

    clickButton(driver, "search-submit");

    driver.findElement({css:'span[id="individual-label"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'individual-label'");});
    driver.findElement({css:'div[role="alert"] button[class="btn btn-default btn-sm"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Recovery' button");});
    check(driver, 1);

    driver.quit();
});
