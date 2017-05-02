var console = require('console');
var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    firstName = ''+Math.round(+new Date()/1000),
    assert = require('assert');

/**
 * Проверка черновиков
 * @param driver
 * @param count - количество черновиков, которое должно быть
 */

function check(driver, count) {
    basic.menu(driver, 'Drafts');
    driver.sleep(basic.FAST_OPERATION);
    driver.findElements({css:'div[id="drafts"] span[typeof="v-s:Person"]'}).then(function(elements_arr){
        if (elements_arr.length > 0) {
            if (count == "true") {
                basic.execute(driver, 'click', 'div[id="drafts"] span[typeof="v-s:Person"]', "Cannot click on selected draft");
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

/**
 * Заполнение поля данными
 * @param driver
 * @param property - поле
 * @param something - данные
 */

function fillProperty(driver, property, something) {
    basic.execute(driver, 'sendKeys', '[property="v-s:' + property + '"] + veda-control input',
        "Cannot fill 'v-s:" + property + "' for person", something);
}

/**1.Open Page -> Login(as karpovrt);
 * 2.Open create person document form -> Edit first and last name -> Save as draft -> Check data
 * 3.Check number of drafts(must be 1);
 * 4.Open draft -> Edit(add middle name and Date) -> Save as document;
 * 5.Check number of drafts(must be 0);
 * 6.Quit;
 *
 * 1.Открываем Страницу -> Заходим в систему под karpovrt;
 * 2.Открываем форму создания Персоны -> Вводим Фамилию и Имя -> Отправляем в черновик -> Проверяем, правильно ли сохранилась
 * персона в черновике;
 * 3.Проверяем количество черновиков(должно быть 1);
 * 4.Заходим в созданный черновик -> Редактируем его(Добавляем Отчество и Дату рождения) -> Сохраняем;
 * 5.Проверяем, что черновиков 0;
 * 6.Выход.
 */


basic.getDrivers().forEach(function(drv) {
    console.time("testDrafts");

    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    basic.openCreateDocumentForm(driver, 'Персона', 'v-s:Person');
    var lastName = 'Draft';
    fillProperty(driver, 'lastName', lastName);
    fillProperty(driver, 'firstName', firstName);
    driver.executeScript("$('div[typeof=\"v-s:Person\"] > .action#draft')[0].scrollIntoView(true);");
    basic.isEnabled(driver, '#draft', basic.FAST_OPERATION);
    basic.execute(driver, 'click', '#draft', "Cannot click on 'draft' button");
    driver.findElement({css:'div[property="v-s:firstName"] span[class="value-holder"]'}).getText().then(function (txt) {
        assert(txt == firstName);
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems that person is not saved properly/FN");});
    driver.findElement({css:'div[property="v-s:lastName"] span[class="value-holder"]'}).getText().then(function (txt) {
        assert(txt == lastName);
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems that person is not saved properly/LN");});

    //Проверям наличие его в наших черновиках
    check(driver, "true");

    //Досоздаем черновик
    fillProperty(driver, 'middleName', 'Пупкин');
    var now = new Date();
    fillProperty(driver, 'birthday',
        now.getFullYear() + '-' + ('0' + (now.getMonth() + 1)).slice(-2) + '-' + ('0' + now.getDate()).slice(-2));
    basic.execute(driver, 'click', '[property="v-s:lastName"] + veda-control input', "Cannot click on 'last name control' for person");
    //Сохраняем его как нормальный документ
    driver.executeScript("$('div[typeof=\"v-s:Person\"] > .action#save')[0].scrollIntoView(true);");
    basic.execute(driver, 'click', '#save', "Cannot click on 'save' button");

    check(driver, "false");

    console.timeEnd("testDrafts");
    driver.quit();
})
