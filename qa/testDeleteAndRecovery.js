var webdriver = require('selenium-webdriver'),
    timeStamp = ''+Math.round(+new Date()/1000),
    basic = require('./basic.js');

/**
 * Проверка элементов в результате поиска
 * @param driver
 * @param count - необходимое количество элементов
 */

function check(driver, count) {
    basic.openFulltextSearchDocumentForm(driver, 'Стартовая форма', 'v-wf:StartForm');
    basic.execute(driver, 'clear', 'h4[about="v-fs:EnterQuery"]+div[class="form-group"] input', '', '');
    basic.execute(driver, 'sendKeys', 'h4[about="v-fs:EnterQuery"]+div[class="form-group"] input',
        "Cannot input search request", timeStamp);
    basic.execute(driver, 'click', 'button[id="submit"]', "Cannot click on 'submit' button");
    driver.wait
    (
        function () {
            basic.execute(driver, 'click', 'div[id="fulltext-search"] a[id="refresh"]', "Cannot click on 'refresh' button");
            driver.sleep(basic.FAST_OPERATION);
            return driver.findElement({css:'span[href="#params-ft"]+span[class="badge"]'}).getText().then(function (txt) {
                return txt == count;
            });
        },
        basic.EXTRA_SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Number of elements is wrong, expected: " + count);});
}

/**
 * Кликаем по кнопке
 * @param driver
 * @param button - кнопка, по которой необходимо кликнуть
 */

function clickButton(driver, button) {
    driver.executeScript("document.querySelector('button[id="+button+"]').scrollIntoView(true);")
    basic.execute(driver, 'click', 'button[id="'+ button +'"]', "Cannot click on "  + button +  " button");
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
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    basic.openCreateDocumentForm(driver, 'Стартовая форма', 'v-wf:StartForm');
    driver.executeScript("document.querySelector('strong[about=\"rdfs:label\"]').scrollIntoView(true);");
    basic.execute(driver, 'click', 'veda-control[data-type="multilingualString"]', "Cannot click on 'rdfs:label' field");
    basic.execute(driver, 'sendKeys', 'veda-control[data-type="multilingualString"] input[type="text"]', "Cannot fill 'rdfs:label' field", timeStamp);
    clickButton(driver, "save");
    driver.sleep(basic.FAST_OPERATION);

    check(driver, 1);
    basic.execute(driver, 'click', 'span[typeof="v-wf:StartForm"]', "Cannot click on 'StartForm' button");
    clickButton(driver, "delete");
    driver.switchTo().alert().accept();

    check(driver, 0);

    basic.menu(driver, 'Search');
    basic.execute(driver, 'sendKeys', '#q', 'Cannot fill input field', "'rdfs:label' == '"+ timeStamp + "' && 'v-s:deleted' == 'true'");
    clickButton(driver, "search-submit");
    basic.execute(driver, 'click', 'span[id="individual-label"]', "Cannot click on 'individual-label'");
    basic.execute(driver, 'click', 'div[role="alert"] button[class="btn btn-default btn-sm"]', "Cannot click on 'Recovery' button");
    check(driver, 1);

    driver.quit();
});
