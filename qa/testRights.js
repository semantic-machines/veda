var webdriver = require('selenium-webdriver'),
    person = require('./person.js'),
    basic = require('./basic.js'),
    timeStamp = ''+Math.round(+new Date()/1000);

/**
 * Поиск
 * @param driver
 * @param somethingUnique - имя шаблона, документ которого ищется
 * @param count - количество документов, ожидаемое по окончанию поиска
 */

function search(driver, somethingUnique, count) {
    basic.execute(driver, 'click', 'a[id="params-pill-ft"]', "Cannot click on 'params-pill-ft' button");
    basic.execute(driver, 'sendKeys', 'h4[about="v-fs:EnterQuery"]+div[class="form-group"] input',
        "Cannot input search request", somethingUnique);
    driver.wait
    (
        function () {
            basic.execute(driver, 'click', 'h4[about="v-fs:EnterQuery"]+div[class="form-group"] button[id="submit"]',
                "Cannot click on 'submit' button");
            driver.sleep(basic.FAST_OPERATION);
            return driver.findElement({css:'span[href="#params-ft"]+span[class="badge"]'}).getText().then(function (txt) {
                return txt == count;
            });
        },
        basic.SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Number of documents is incorrect, expected: " + count);});
}
/**
 * 1.Open page -> Login(as karpovrt);
 * 2.Create Person1 -> Open fulltext search document form -> Search Person1 -> result: 1 document;
 * 3.Logout -> Login(as bychinat);
 * 4.Search Person1 -> Result: 0 document;
 * 5.Create Person2 -> Logout;
 * 6.Login(as karpovrt) -> Search Person2 -> Result: 1 document
 *
 * 1.Открываем страницу -> Входим в систему под karpovrt;
 * 2.Создаем Персону1 -> Открываем форму полнотекстового поиска -> Проверяем, что находится созданная Персона1;
 * 3.Выходим из системы -> Входим в систему под bychinat;
 * 4.Ищем Персону1 -> Проверяем, что ничего не находится;
 * 5.Cоздаем Персону2 -> Выходим из системы;
 * 6.Входим в систему под karpovrt -> Ищем Персону2 -> Проверяем, что она находится;
 * 7.Выход;
*/

basic.getDrivers().forEach (function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    //Создание документа пользователем с большими правами
    var now = new Date();
    person.createPerson(driver, drv, 'Иванов', 'Иван', timeStamp, ('0' + now.getDate()).slice(-2) + '.' + ('0' + (now.getMonth() + 1)).slice(-2) + '.' + now.getFullYear());
    basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
    search(driver, timeStamp, 1);

    //Проверка отсутсвия созданного документа и создание нового документа пользователем с меньшими правами
    basic.logout(driver);
    basic.login(driver, 'bychinat', '123', '4', 'Администратор4');

    basic.execute(driver, 'click', 'a[id="params-pill-ft"]', "Cannot click on 'params-pill-ft' button");
    basic.execute(driver, 'clear', 'div[typeof="v-fs:FulltextRequest"] input[id="fulltext"]', "Cannot find 'fulltext' field");
    basic.execute(driver, 'sendKeys', 'div[typeof="v-fs:FulltextRequest"] input[id="fulltext"]', "Cannot fill 'fulltext' field", 'Персона');
    search(driver, timeStamp, 0);

    person.createPerson(driver, drv, 'Иванов', 'Иван', timeStamp + 1, ('0' + now.getDate()).slice(-2) + '.' + ('0' + (now.getMonth() + 1)).slice(-2) + '.' + now.getFullYear());
    basic.logout(driver);

    //Проверка наличия созданного документа пользователем с меньшими правами
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
    search(driver, timeStamp + 1, 1);

    driver.quit();
});

