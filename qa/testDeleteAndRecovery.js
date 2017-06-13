var webdriver = require('selenium-webdriver'),
    timeStamp = ''+Math.round(+new Date()/1000),
    basic = require('./basic.js');

/**
 * Проверка элементов в результате поиска
 * @param driver
 * @param count - необходимое количество элементов
 */

function check(driver, count, phase) {
    basic.openFulltextSearchDocumentForm(driver, 'Стартовая форма', 'v-wf:StartForm');
    basic.execute(driver, 'clear', 'h4[about="v-fs:EnterQuery"]+div[class="form-group"] input', '', '');
    basic.execute(driver, 'sendKeys', 'h4[about="v-fs:EnterQuery"]+div[class="form-group"] input',
        "****** PHASE#" + phase + " : ERROR = Cannot input search request", timeStamp);
    basic.execute(driver, 'click', 'button[id="submit"]', "Cannot click on 'submit' button");
    driver.wait
    (
        function () {
            basic.execute(driver, 'click', 'div[id="fulltext-search"] a[id="refresh"]', "****** PHASE#" + phase + " : ERROR = Cannot click on 'refresh' button");
            driver.sleep(basic.FAST_OPERATION);
            return driver.findElement({css:'span[href="#params-ft"]+span[class="badge"]'}).getText().then(function (txt) {
                return txt == count;
            });
        },
        basic.EXTRA_SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Number of elements is wrong, expected: " + count);});
}

/**
 * Кликаем по кнопке
 * @param driver
 * @param button - кнопка, по которой необходимо кликнуть
 */

function clickButton(driver, button, phase) {
    driver.sleep(basic.FAST_OPERATION);
    driver.executeScript("document.querySelector('button[id="+button+"]').scrollIntoView(true);");
    basic.execute(driver, 'click', 'button[id="'+ button +'"]', "****** PHASE#" + phase + " : ERROR = Cannot click on "  + button +  " button");
    driver.sleep(basic.FAST_OPERATION);
}

/**
 * 1.Open page -> Login(as karpovrt);
 * 2.Open create StartForm document form -> Change label -> Save StartForm; 
 * 3.Search StartForm -> Open StartForm -> Delete StartForm;
 * 4.Check, can't search StartForm; 
 * 5.Search deleted StartForm with special commandline -> Recovery StarftForm ->
 * -> Check, can search StartForm after recovery;
 * 6.Quit;
 *
 * 1.Открываем страницу -> Входим в систему под karpovrt;
 * 2.Открываем форму создания Стартовой формы -> Вводим название -> Сохраняем;
 * 3.Ищем созданную Стартовую форму -> Открываем ее -> Удаляем Стартовую форму;
 * 4.Проверяем, что теперь созданную Стартовую форму не найти;
 * 5.Вводим специальный запрос в поиске, чтобы найти удаленную Стартовую форму ->  Восстанавливаем ее ->
 * -> Проверяем, что она появилась в поиске;
 * 6.Выход;
 */

basic.getDrivers().forEach(function(drv){
    //PHASE#1
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    driver.findElement({css:'#app'}).then(function(){console.log("****** PHASE#1 > LOGIN              : COMPLETE");});

    //PHASE#2
    basic.openCreateDocumentForm(driver, 'Стартовая форма', 'v-wf:StartForm');
    driver.executeScript("document.querySelector('strong[about=\"rdfs:label\"]').scrollIntoView(true);");
    basic.execute(driver, 'click', 'veda-control[data-type="multilingualString"]',
        "****** PHASE#2 > CREATE STARTFORM : ERROR = Cannot click on 'rdfs:label' field");
    basic.execute(driver, 'sendKeys', 'veda-control[data-type="multilingualString"] input[type="text"]',
        "****** PHASE#2 > CREATE STARTFORM : ERROR = Cannot fill 'rdfs:label' field", timeStamp);
    clickButton(driver, "save", 2);
    driver.sleep(basic.FAST_OPERATION);
    driver.findElement({css:'#app'}).then(function(){console.log("****** PHASE#2 > CREATE STARTFORM   : COMPLETE");});

    //PHASE#3
    check(driver, 1, 3);
    basic.execute(driver, 'click', 'span[typeof="v-wf:StartForm"]', "****** PHASE#3 > DELETE STARTFORM : ERROR = Cannot click on 'StartForm' button");
    clickButton(driver, "delete", 3);
    driver.switchTo().alert().accept();
    driver.findElement({css:'#app'}).then(function(){console.log("****** PHASE#3 > DELETE STARTFORM   : COMPLETE");});

    //PHASE#4
    check(driver, 0, 4);
    driver.findElement({css:'#app'}).then(function(){console.log("****** PHASE#4 > SEARCH STARTFORM   : COMPLETE");});

    //PHASE#5
    basic.menu(driver, 'Search');
    basic.execute(driver, 'sendKeys', '#q', '****** PHASE#5 > RECOVERY STARTFORM : ERROR = Cannot fill input field',
        "'rdfs:label' == '"+ timeStamp + "' && 'v-s:deleted' == 'true'");
    clickButton(driver, "search-submit", 5);
    basic.execute(driver, 'click', 'span[id="individual-label"]', "****** PHASE#5 > RECOVERY STARTFORM : ERROR = Cannot click on 'individual-label'");
    basic.execute(driver, 'click', 'div[role="alert"] button[class="btn btn-default btn-sm"]',
        "****** PHASE#5 > RECOVERY STARTFORM : ERROR = Cannot click on 'Recovery' button");
    check(driver, 1, 5);
    driver.findElement({css:'#app'}).then(function(){console.log("****** PHASE#5 > RECOVERY STARTFORM : COMPLETE");});

    //PHASE#6
    driver.quit();
});
