var webdriver = require('selenium-webdriver'),
    person = require('./person.js'),
    basic = require('./basic.js');

/**
 * Поиск
 *
 * @param templateName - поля аттрибутивного поиска "Персона", по которому необходимо произвести поиск
 * @param somethingUnigue - строки, по которой необходимо произвести поиск
 * @param count - количество элементов, ожидаемое по окончанию поиска
 */


function search(driver, templateName, somethingUnique, count) {
    //Поиск нужного документа
    driver.sleep(basic.FAST_OPERATION);
    for (var i = 0; i < templateName.length; i++) {
        (templateName[i] == 'rdfs:label') ? driver.findElement({css: 'div[id="form-holder"] [property="' + templateName[i] + '"] + veda-control input'}).sendKeys(somethingUnique[i])
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill " + templateName[i] + " field")})
            : driver.findElement({css: '[property="' + templateName[i] + '"] + veda-control input'}).sendKeys(somethingUnique[i])
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill " + templateName[i] + " field")});
    }
    driver.sleep(basic.FAST_OPERATION);
    driver.executeScript("document.querySelector('#find').scrollIntoView(true);");
    driver.findElement({css:'#find'}).click()
       .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Find` button")});
    driver.wait
    (
        function () {
            driver.findElement({css:'div[id="attributive-search"] a[id="refresh"]'}).click()
                .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'refresh' button")});
            driver.sleep(basic.FAST_OPERATION);
            return driver.findElement({css:'span[href="#params-at"]+span[class="badge"]'}).getText().then(function (txt) {
                return txt >= count;
            });
        },
        basic.EXTRA_SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Number of documents is incorrect, expected: " + count)});
    driver.findElement({id:'params-pill-at'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'params-pill-at' button")});
    for (var i = 0; i < templateName.length; i++) {
        (templateName[i] == 'rdfs:label') ? driver.findElement({css: 'div[id="form-holder"] [property="' + templateName[i] + '"] .glyphicon-remove'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot remove old" + templateName[i] + "value")})
            : driver.findElement({css: '[property="' + templateName[i] + '"] .glyphicon-remove'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot remove old" + templateName[i] + "value")});
    }
}

basic.getDrivers().forEach(function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);

    basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');

    //Создание документов типа "Персона"
    var first = "", last = "", middle = "";
    var possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
    for( var i = 0; i < 5; i++ ) {
        first += possible.charAt(Math.floor(Math.random() * possible.length));
        last += possible.charAt(Math.floor(Math.random() * possible.length));
        middle += possible.charAt(Math.floor(Math.random() * possible.length));
    }
    person.createPerson(driver, drv, 'a' + last, first + 'bcc', 'T' + middle);
    person.createPerson(driver, drv, last + 'b', first + 'cbb', middle + 'Q');

    // Открываем Аттрибутивный поиск
    driver.findElement({css:'li[id="menu"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button")});
    driver.wait
    (
        webdriver.until.elementIsVisible(driver.findElement({css:'li[id="menu"] li[resource="v-l:Find"]'})),
        basic.FAST_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Seems there is no `find` button inside settings")});
    driver.findElement({css:'li[id="menu"] li[resource="v-l:Find"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `find` button")});
    driver.wait
    (
        webdriver.until.elementIsVisible(driver.findElement({css:'div[resource="v-fs:Search"]'})),
        basic.FAST_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Search template was not opened")});
    driver.findElement({css:'a[href*="attributive-search"'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Attributive` button")});
    driver.findElement({css:'div[typeof="v-fs:AttributiveRequest"] input[id="fulltext"]'}).sendKeys('Персона')
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot input templateName")});
    driver.wait
    (
        function () {
            return driver.findElements({css:'veda-control.fulltext div.tt-suggestion>p'}).then(function (suggestions) {
                return webdriver.promise.filter(suggestions, function(suggestion) {
                    return suggestion.getText().then(function(txt){ return txt == 'Персона' });
                }).then(function(x) { return x.length>0; });
            });
        },
        basic.FAST_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Dropdown doesnt contains value `"+'Персона'+"`")});
    driver.findElements({css:"veda-control.fulltext div.tt-suggestion>p"}).then(function (suggestions) {
        webdriver.promise.filter(suggestions, function(suggestion) {
            return suggestion.getText().then(function(txt){ return txt == 'Персона' });
        }).then(function(x) { x[0].click();});
    }).thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `"+'Персона'+"` from dropdown")});
    //var possiblefield = ['v-s:firstName', 'v-s:lastName', 'v-s:middleName', 'v-s:birthday', 'rdfs:label'];
    var now = new Date();
    var birthday = now.getFullYear() + '-' + ('0' + (now.getMonth() + 1)).slice(-2) + '-' + ('0' + now.getDate()).slice(-2);
    search(driver, ['v-s:lastName'], [last], 1);
    search(driver, ['v-s:lastName'], ["a" + last], 1);
    search(driver, ['v-s:firstName'], [first], 2);
    search(driver, ['v-s:firstName'], [first.substring(0,3) + "*"], 2);
    search(driver, ['v-s:firstName'], [first + "bcc"], 1);
    search(driver, ['v-s:firstName'], [first + "ccc"], 0);
    search(driver, ['v-s:middleName'], [middle], 1);
    search(driver, ['v-s:middleName'], [middle + "Q"], 1);
    search(driver, ['v-s:birthday'], [birthday], 2);
    search(driver, ['rdfs:label'], [first], 2);
    search(driver, ['rdfs:label'], [middle], 1);
    search(driver, ['rdfs:label', 'v-s:lastName'], [first, last], 1);
    search(driver, ['v-s:firstName', 'v-s:birthday'], [first, birthday], 2);
    search(driver, ['v-s:lastName', 'v-s:firstName', 'v-s:middleName', 'v-s:birthday', 'rdfs:label'],
         [last, first, middle, birthday, first], 1);
    driver.sleep(basic.FAST_OPERATION);
    
    driver.quit();
})