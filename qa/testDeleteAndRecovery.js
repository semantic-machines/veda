var basic = require('./basic.js'),
    timeStamp = ''+Math.round(+new Date()/1000);



/**
 * Проверка элементов в результате поиска
 * @param driver
 * @param count - необходимое количество элементов
 * @param phase - текущая фаза теста
 */

function check(driver, count, phase) {
    basic.openFulltextSearchDocumentForm(driver, 'Стартовая форма', 'v-wf:StartForm', phase);
    basic.execute(driver, 'clear', 'h4[about="v-fs:EnterQuery"]+div[class="form-group"] input', '', '');
    basic.execute(driver, 'sendKeys', 'h4[about="v-fs:EnterQuery"]+div[class="form-group"] input',
        "****** PHASE#" + phase + " : ERROR = Cannot input search request", timeStamp);
    basic.execute(driver, 'click', 'button[about="v-fs:Find"]', "****** PHASE#" + phase + " : ERROR = Cannot click on 'submit' button");
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
 * @param phase - текущая фаза теста
 */

function clickButton(driver, button, phase) {
    driver.sleep(basic.FAST_OPERATION);
    driver.executeScript("document.querySelector('button[id="+button+"]').scrollIntoView(true);")
        .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Cannot scroll to " + button + " button");});
    basic.execute(driver, 'click', 'button[id="'+ button +'"]', "****** PHASE#" + phase + " : ERROR = Cannot click on "  + button +  " button");
    driver.sleep(basic.FAST_OPERATION);
}

/**
 * 0.Open page -> Login(as karpovrt);
 * 1.Open create StartForm document form -> Input label -> Save StartForm;
 * 2.Search StartForm -> Open StartForm -> Delete StartForm;
 * 3.Cannot search StartForm;
 * 4.Search deleted StartForm with special commandline -> Recovery StartForm ->
 * -> Can search StartForm after recovery;
 *
 * 0.Открываем страницу -> Входим в систему под karpovrt;
 * 1.Открываем форму создания Стартовой формы -> Вводим название -> Сохраняем;
 * 2.Ищем созданную Стартовую форму -> Открываем ее -> Удаляем Стартовую форму;
 * 3.Проверяем, что теперь созданную Стартовую форму не найти;
 * 4.Вводим специальный запрос в поиске, чтобы найти удаленную Стартовую форму ->  Восстанавливаем ее ->
 * -> Проверяем, что она появилась в поиске;
 */

basic.getDrivers().forEach(function(drv){
    console.log("testDeleteAndRecovery.js");

    //PHASE#0: Login
    var driver = basic. getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 0);

    //PHASE#1: New Startform
    basic.openCreateDocumentForm(driver, 'Стартовая форма', 'v-wf:StartForm', 1);
    driver.executeScript("document.querySelector('strong[about=\"rdfs:label\"]').scrollIntoView(true);")
        .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#1 : ERROR = Cannot scroll to 'rdfs:label' field");});
    basic.execute(driver, 'click', 'veda-control[data-type="multilingualString"]',
        "****** PHASE#1 > CREATE : ERROR = Cannot click on 'rdfs:label' field");
    basic.execute(driver, 'sendKeys', 'veda-control[data-type="multilingualString"] input[type="text"]',
        "****** PHASE#1 > CREATE : ERROR = Cannot fill 'rdfs:label' field", timeStamp);
    clickButton(driver, "save", 1);
    driver.sleep(basic.FAST_OPERATION);

    //PHASE#2: Delete
    check(driver, 1, 2);
    basic.execute(driver, 'click', 'span[typeof="v-wf:StartForm"]', "****** PHASE#2 > DELETE : ERROR = Cannot click on 'StartForm' button");
    clickButton(driver, "delete", 2);
    driver.switchTo().alert().accept();

    //PHASE#3: Checking
    check(driver, 0, 3);

    //PHASE#4: Recovery
    basic.menu(driver, 'Search', 4);
    basic.execute(driver, 'sendKeys', '#q', '****** PHASE#4 > RECOVERY : ERROR = Cannot fill input field',
        "'rdfs:label' == '"+ timeStamp + "' && 'v-s:deleted' == 'true'");
    clickButton(driver, "search-submit", 5);
    basic.execute(driver, 'click', 'span[id="individual-label"]', "****** PHASE#4 > RECOVERY : ERROR = Cannot click on 'individual-label'");
    basic.execute(driver, 'click', '#deleted-alert .recover',
        "****** PHASE#4 > RECOVERY : ERROR = Cannot click on 'Recovery' button");
    check(driver, 1, 5);
    driver.quit();
});
