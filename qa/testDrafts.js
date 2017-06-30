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

function check(driver, count, phase) {
    basic.menu(driver, 'Drafts', phase);
    driver.sleep(basic.FAST_OPERATION);
    driver.findElements({css:'div[id="drafts"] [typeof="v-s:Person"]'}).then(function(elements_arr){
        if (elements_arr.length > 0) {
            if (count == "true") {
                basic.execute(driver, 'click', 'div[id="drafts"] [typeof="v-s:Person"]',
                    "****** PHASE#" + phase + " > CHECK DRAFT : ERROR = Cannot click on selected draft");
            }
            if (count == "false") {
                console.trace("****** PHASE#" + phase + " > CHECK DRAFT : ERROR = Expected number of drafts is 0, but get 1");
                process.exit(1);
            }
        }
        if (elements_arr.length === 0){
            if (count == "true") {
                console.trace("****** PHASE#" + phase + " > CHECK DRAFT : ERROR = Expected number of drafts is 1, but get 0");
                process.exit(1);
            }
        }
    }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " > CHECK DRAFT : ERROR = Seems there is no `drafts` field");});
}

/**
 * Заполнение поля данными
 * @param driver
 * @param property - поле
 * @param something - данные
 */

function fillProperty(driver, property, something, phase) {
    basic.execute(driver, 'sendKeys', '[property="v-s:' + property + '"] + veda-control input',
        "****** PHASE#" + phase + " : ERROR = Cannot fill 'v-s:" + property + "' for person", something);
}

/**
 * 0.Open Page -> Login(as karpovrt);
 * 1.Open create person document form -> Edit first and last name -> Save as draft#1 -> Check person data
 * 2.Check number of drafts(must be 1);
 * 3.Open draft#1 -> Edit(add middle name and Date) -> Save as document#1;
 * 4.Check number of drafts(must be 0);
 *
 * 0.Открываем Страницу -> Заходим в систему под karpovrt;
 * 1.Открываем форму создания Персоны -> Вводим Фамилию и Имя -> Сохраняем как черновик#1 -> Проверяем, правильно ли сохранилась
 * персона в черновике#1;
 * 2.Проверяем, что черновиков 1;
 * 3.Заходим в созданный черновик#1 -> Редактируем его(Добавляем Отчество и Дату рождения) -> Сохраняем как документ#1;
 * 4.Проверяем, что черновиков 0;
 */


basic.getDrivers().forEach(function(drv) {
    //PHASE#0: Вход
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    //PHASE#1:Создаем черновик
    basic.openCreateDocumentForm(driver, 'Персона', 'v-s:Person', 1);
    var lastName = 'Draft';
    fillProperty(driver, 'lastName', lastName, 1);
    fillProperty(driver, 'firstName', firstName, 1);
    driver.executeScript("$('div[typeof=\"v-s:Person\"] > .action#draft')[0].scrollIntoView(true);");
    basic.isEnabled(driver, '#draft', basic.FAST_OPERATION, 1);
    basic.execute(driver, 'click', '#draft', "****** PHASE#1 > NEW DRAFT : ERROR = Cannot click on 'draft' button");
    driver.findElement({css:'div[property="v-s:firstName"] span[class="value-holder"]'}).getText().then(function (txt) {
        assert(txt == firstName);
    }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#1 > NEW DRAFT : ERROR = Seems that person is not saved properly/FN");});
    driver.findElement({css:'div[property="v-s:lastName"] span[class="value-holder"]'}).getText().then(function (txt) {
        assert(txt == lastName);
    }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#1 > NEW DRAFT : ERROR = Seems that person is not saved properly/LN");});

    //PHASE#2: Проверям наличие его в наших черновиках
    check(driver, "true", 2);

    //PHASE#3: Досоздаем черновик в документ
    fillProperty(driver, 'middleName', 'Пупкин', 3);
    var now = new Date();
    fillProperty(driver, 'birthday',
        now.getFullYear() + '-' + ('0' + (now.getMonth() + 1)).slice(-2) + '-' + ('0' + now.getDate()).slice(-2), 3);
    basic.execute(driver, 'click', '[property="v-s:lastName"] + veda-control input',
        "****** PHASE#3 > EDIT DRAFT : ERROR = Cannot click on 'last name control' for person");
    driver.executeScript("$('div[typeof=\"v-s:Person\"] > .action#save')[0].scrollIntoView(true);");
    basic.execute(driver, 'click', '#save', "****** PHASE#3 > EDIT DRAFT : ERROR = Cannot click on 'save' button");

    //PHASE#4: Проверяем, что теперь нет черновика
    check(driver, "false", 4);
    driver.quit();
});
