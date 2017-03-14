var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    person = require('./person.js'),
    delegationRequest = require('./delegationRequest.js'),
    timeStamp = ''+Math.round(+new Date()/1000);

/**
 * Поиск элементов
 * @param driver
 * @param somethingUnique - элемент
 * @param count - количество, которое должно быть после поиска
*/

function check(driver, somethingUnique, count) {
    basic.execute(driver, 'click', 'a[id="params-pill-ft"]', "Cannot click on 'params-pill-ft' button");
    basic.execute(driver, 'sendKeys', 'h4[about="v-fs:EnterQuery"]+div[class="form-group"] input',
        "Cannot input search request", somethingUnique);
    driver.wait
    (
        function () {
            driver.findElement({css:'h4[about="v-fs:EnterQuery"]+div[class="form-group"] button[id="submit"]'}).click();
            driver.sleep(basic.FAST_OPERATION);
            return driver.findElement({css:'span[href="#params-ft"]+span[class="badge"]'}).getText().then(function (txt) {
                return txt == count;
            });
        },
        basic.EXTRA_SLOW_OPERATION
    ).thenCatch(function (e) {basic.errorHandler(e, "Number of documents is incorrect, expected: " + count);});
}

/**
 * 1.Open page -> login(as karpovrt)
 * 2.Create person1 -> Logout
 * 3.Login(as bychinat) -> Check number of persons(0) -> Logout;
 * 4.Login(as kaprovrt) -> Create delegation request -> Logout;
 * 5.Login(as bychinat) -> Check number of persons(1) -> Logout;
 * 6.Quit;
 * 
 * 1.Открываем страницу -> Входим в систему под karpovrt;
 * 2.Создаем Персону1 -> Выходи из системы;
 * 3.Входим в систему под bychinat -> Проверяем, что в поиске не ищется наша Персона1 -> Выходим из системы;
 * 4.Входим в систему под karpovrt -> Создаем делегирование -> Выходим из системы;
 * 5.Входим в систему под bychinat -> Проверяем, что после делегирования наша Персона1 ищется -> Выходим из системы;
 * 6.Выход;
*/

basic.getDrivers().forEach(function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    var now = new Date();
    person.createPerson(driver, drv, 'Bourne', 'Jason', timeStamp,
        ('0' + now.getDate()).slice(-2) + '.' + ('0' + (now.getMonth() + 1)).slice(-2) + '.' + now.getFullYear());
    basic.logout(driver);

    basic.login(driver, 'bychinat', '123', '4', 'Администратор4');
    basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
    check(driver,timeStamp, 0);
    basic.logout(driver);

    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    delegationRequest.createRequestDelegation(driver, 'Администратор4', 'Администратор4 : Аналитик');
    driver.sleep(basic.FAST_OPERATION);
    basic.execute(driver, 'click', 'a[href="#/v-l:Welcome"]', "Cannot click on 'Welcome' button");
    basic.logout(driver);

    basic.login(driver, 'bychinat', '123', '4', 'Администратор4');
    basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
    check(driver, timeStamp, 1);

    driver.quit();
});
