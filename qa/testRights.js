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

function search(driver, somethingUnique, count, phase) {
    basic.execute(driver, 'click', 'a[id="params-pill-ft"]', "****** PHASE#" + phase + " : ERROR = Cannot click on 'params-pill-ft' button");
    basic.execute(driver, 'sendKeys', 'h4[about="v-fs:EnterQuery"]+div[class="form-group"] input',
        "****** PHASE#" + phase + " : ERROR = Cannot input search request", somethingUnique);
    driver.wait
    (
        function () {
            basic.execute(driver, 'click', 'h4[about="v-fs:EnterQuery"]+div[class="form-group"] button[id="submit"]',
                "****** PHASE#" + phase + " > SEARCH PERSON" + (count + 1) + " : ERROR = Cannot click on 'submit' button");
            driver.sleep(basic.FAST_OPERATION);
            return driver.findElement({css:'span[href="#params-ft"]+span[class="badge"]'}).getText().then(function (txt) {
                return txt == count;
            });
        },
        basic.SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " > SEARCH PERSON" + (count + 1) + " : ERROR = Number of documents is incorrect, expected: " + count);});
}
/**
 * 0.Open page -> Login(as karpovrt);
 * 1.Create Person1;
 * 2.Logout -> Login(as bychinat) -> Search Person1 -> Result: 0 document;
 * 3.Create Person2 -> Logout;
 * 4.Login(as karpovrt) -> Search Person2 -> Result: 1 document;
 *
 * 0.Открываем страницу -> Входим в систему под karpovrt;
 * 1.Создаем Персону1
 * 2.Выходим из системы -> Входим в систему под bychinat -> Ищем Персону1 -> Проверяем, что ничего не находится;
 * 3.Cоздаем Персону2 -> Выходим из системы;
 * 4.Входим в систему под karpovrt -> Ищем Персону2 -> Проверяем, что она находится;
*/

basic.getDrivers().forEach (function (drv) {
    //PHASE#0: Login
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 0);

    //PHASE#1: Create document(by karpovrt)
    var now = new Date();
    person.createPerson(driver, drv, 'Иванов', 'Иван', timeStamp, ('0' + now.getDate()).slice(-2) + '.' + ('0' + (now.getMonth() + 1)).slice(-2) + '.' + now.getFullYear());

    //PHASE#2: Check
    basic.logout(driver, 2);
    basic.login(driver, 'bychinat', '123', '4', 'Администратор4', 2);
    basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person', 2);
    search(driver, timeStamp, 0, 2);

    //PHASE#3: Create document(by bychinat)
    person.createPerson(driver, drv, 'Иванов', 'Иван', timeStamp + 1, ('0' + now.getDate()).slice(-2) + '.' + ('0' + (now.getMonth() + 1)).slice(-2) + '.' + now.getFullYear());
    basic.logout(driver);

    //PHASE#4: Check
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 4);
    basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person', 4);
    search(driver, timeStamp + 1, 1, 4);
    driver.quit();
});

