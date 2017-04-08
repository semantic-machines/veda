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
    for (var i = 0; i < templateName.length; i++) {
        (templateName[i] == 'rdfs:label') ? basic.execute(driver, 'sendKeys', 'div[id="form-holder"] [property="' + templateName[i] + '"] + veda-control input',
                "Cannot fill " + templateName[i] + " field", somethingUnique[i])
            : basic.execute(driver, 'sendKeys', '[property="' + templateName[i] + '"] + veda-control input',
                "Cannot fill " + templateName[i] + " field", somethingUnique[i]);
    }
    driver.executeScript("document.querySelector('#find').scrollIntoView(true);");
    basic.execute(driver, 'click', '#find', "Cannot click on `Find/Найти` button");
    driver.wait
    (
        function () {
            basic.execute(driver, 'click', 'div[id="attributive-search"] a[id="refresh"]', "Cannot click on 'refresh' button");
            driver.sleep(basic.FAST_OPERATION);
            return driver.findElement({css:'span[href="#params-at"]+span[class="badge"]'}).getText().then(function (txt) {
                return txt >= count;
            });
        },
        basic.SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Number of documents is incorrect, expected: " + count);});
    basic.execute(driver, 'click', 'a[id="params-pill-at"]', "Cannot click on 'params-pill-at' button");
    for (var i = 0; i < templateName.length; i++) {
        (templateName[i] == 'rdfs:label') ? basic.execute(driver, 'click', 'div[id="form-holder"] [property="' + templateName[i] + '"] .glyphicon-remove', "Cannot remove old" + templateName[i] + "value", '')
            : basic.execute(driver, 'click', '[property="' + templateName[i] + '"] .glyphicon-remove', "Cannot remove old" + templateName[i] + "value", '')
    }
}

/**
 * 1.Open page -> login(as karpovrt);
 * 2.Create Person1 -> Create Person2;
 * 3.Open attributive search person form -> Search attributive requests and check results;
 * 4.Quit;
 *
 * 1.Открываем страницу -> Входим в систему под kaprovrt;
 * 2.Создаем Персону1 -> Создаем Персону2;
 * 3.Открываем аттрибутивную форму поиска -> Делаем запросы на поиск Персон по аттрибутам и проверяем верен ли результат;
 * 4.Выход
*/


basic.getDrivers().forEach(function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    //Создание документов типа "Персона"
    var first = "xGIo5f", last = "GhiOJe", middle = "NE1UCD";
    var now = new Date();
    person.createPerson(driver, drv, 'a' + last, first + 'bcc', 'T' + middle,
        ('0' + now.getDate()).slice(-2) + '.' + ('0' + (now.getMonth() + 1)).slice(-2) + '.' + now.getFullYear());
    person.createPerson(driver, drv, last + 'b', first + 'cbb', middle + 'Q',
        ('0' + now.getDate()).slice(-2) + '.' + ('0' + (now.getMonth() + 1)).slice(-2) + '.' + now.getFullYear());

    // Открываем Аттрибутивный поиск
    basic.menu(driver, 'Find');
    basic.isVisible(driver, 'div[resource="v-fs:Search"]', basic.FAST_OPERATION);
    basic.execute(driver, 'click', 'a[href*="attributive-search"]', "Cannot click on `Attributive` button");
    basic.execute(driver, 'sendKeys', 'div[typeof="v-fs:AttributiveRequest"] input[id="fulltext"]',
        "Cannot input templateName", 'Персона');
    driver.sleep(basic.FAST_OPERATION);
    driver.wait
    (
        function () {
            return driver.findElements({css:'veda-control.fulltext div.tt-suggestion>p'}).then(function (suggestions) {
                return webdriver.promise.filter(suggestions, function(suggestion) {
                    return suggestion.getText().then(function(txt){ return txt === 'Персона' });
                }).then(function(x) { return x.length > 0; });
            });
        },
        basic.SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Dropdown doesnt contains value "+'Персона');});
    driver.findElements({css:"veda-control.fulltext div.tt-suggestion>p"}).then(function (suggestions) {
        webdriver.promise.filter(suggestions, function(suggestion) {
            return suggestion.getText().then(function(txt){ return txt == 'Персона';});
        }).then(function(x) { x[0].click();});
    }).thenCatch(function (e) {basic.errorHandler(e, "Cannot click on "+'Персона'+" from dropdown");});
    var birthday = now.getFullYear() + '-' + ('0' + (now.getMonth() + 1)).slice(-2) + '-' + ('0' + now.getDate()).slice(-2);
    search(driver, ['v-s:lastName'], ["a" + last], 1);
    search(driver, ['v-s:firstName'], [first.substring(0,4) + "*"], 2);
    search(driver, ['v-s:firstName'], [first + "ccc"], 0);
    search(driver, ['v-s:middleName'], [middle], 1);
    search(driver, ['v-s:birthday'], [birthday], 2);
    search(driver, ['rdfs:label'], [first], 2);
    search(driver, ['rdfs:label', 'v-s:lastName'], [first, last], 1);
    search(driver, ['v-s:firstName', 'v-s:birthday'], [first, birthday], 2);
    search(driver, ['v-s:lastName', 'v-s:firstName', 'v-s:middleName', 'v-s:birthday', 'rdfs:label'],
         [last, first, middle, birthday, first], 1);

    driver.quit();
})
