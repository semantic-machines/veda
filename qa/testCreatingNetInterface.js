var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js');
/**
 * Удаление текущего элемента
 *@param driver
*/
function deleteElement(driver) {
    basic.execute(driver, 'click', '.delete-state', "Cannot click on 'delete' button");
    driver.switchTo().alert().accept();
}

/**
 * Проверка нахождения элемента
 *@param driver
 *@param element - элемент
*/

function check(driver, element) {
    driver.findElements({css:''+element}).then(function(elements_arr){
        if (elements_arr.length > 0) {
            console.trace("Seems "+ element +" has not deleted");
            process.exit(1);
        }
    }).thenCatch(function (e) {basic.errorHandler(e, "Cannot find any "+ element);});
}

/**
 * Проверка состояни до и после нажатия кнопки
 *@param a - состояние до нажатия кнопки
 *@param b - состояние после нажатия кнопки
 *@param button - кнопка
*/
function equal(a, b, button) {
    if(a == b) {
        console.trace("Seems " + button + " does not work");
        process.exit(1);
    }
}

/**
 * 1.Open page -> login(as karpovrt);
 * 2.Open create net document form;
 * 3.Create flow from input to output -> Delete flow -> Check flow is no on canvas;
 * 4.Create task1 -> Copy task1(task2) -> Delete task1 -> Delete task2 -> Check task is no on canvas;
 * 5.Create condition -> Check condition;
 * 6.Default state(state1) -> Zoom-out(state2) -> Equal state1 & state2;
 * 7.Zoom-in -> Zoom-in(state3) -> Equal state1 & state3;
 * 8.Zoom-default(state4) -> Equal state3 & state4;
 * 9.Full-width -> Check canvas in full-width;
 * 10.Save net -> Check save;
 * 11.Quit;
 *
 * 1.Открываем страницу -> Входим в систему под karpovrt;
 * 2.Открываем форму создания сети;
 * 3.Соединяем стрелкой вход и выход -> удаляем стрелку -> Проверяем удалена ли она;
 * 4.Создаем задачу1 -> Копируем ее(задача2) -> Удаляем задачу1 -> Удаляем задачу2 -> Проверяем, что задач нет;
 * 5.Создаем состояние -> Проверяем создалось ли оно;
 * 6.Обычное состояние(состояние1) -> Уменьшаем масштаб(состояние2) -> Проверяем, что они разные;
 * 7.Увеличиваем масштаб -> Увеличиваем масштаб(состояние3) -> Проверяем, что состояние1 и состояние3 разные;
 * 8.Обычное состояние(состояние4) -> Проверяем, что состояние4 и состояние3 разные;
 * 9.Проверяем, что после нажатия, редактор стал во всю ширину.
 * 10.Сохраняем сеть -> Проверяем сохранение;
 * 11.Выход;
*/


basic.getDrivers().forEach(function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    basic.openCreateDocumentForm(driver, 'Сеть', 'v-wf:Net');
    basic.isVisible(driver, '.workflow-canvas-wrapper', basic.FAST_OPERATION);
    basic.execute(driver, 'click', '.workflow-canvas-wrapper', "Cannot click on net canvas");
    basic.isVisible(driver, 'span[about="v-wf:Net"]', basic.FAST_OPERATION);

    //Создание и удаление коннектора между двумя элементами
    new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-io-condition-input .ep'}), driver.findElement({css:'.glyphicon-stop'})).perform();
    basic.execute(driver, 'click', 'svg[class="_jsPlumb_connector"]', "Cannot click on 'connector' arrow");
    deleteElement(driver);
    check(driver, 'svg[class="_jsPlumb_connector"]');

    // //Создание задачи, клонирование и удаление
    basic.execute(driver, 'click', '.create-task', "Cannot click on 'create-task' button");
    basic.execute(driver, 'click', '.state-task', "Cannot click on 'state-task' button");
    basic.execute(driver, 'click', '.copy-net-element', "Cannot click on 'copy-net-element' button");
    deleteElement(driver);
    basic.execute(driver, 'click', '.state-task', "Cannot click on 'state-task' button");
    deleteElement(driver);
    check(driver, '.state-task');

    basic.execute(driver, 'click', '.create-condition', "Cannot click 'create-condition' button");
    basic.execute(driver, 'click', '.state-condition', "Cannot click 'state-condition' button");

    var a = " ", b = "";
    driver.findElement({css:'div[id="workflow-canvas"]'}).getCssValue("transform").then(function (state) {a = state;})
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
    basic.execute(driver, 'click', '.zoom-out', "Cannot click on 'zoom-out' button");
    driver.findElement({css:'div[id="workflow-canvas"]'}).getCssValue("transform").then(function (state) {b == state;})
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
    equal(a, b, 'zoom-out');

    basic.execute(driver, 'click', '.zoom-in', "Cannot click on 'zoom-in' button");
    basic.execute(driver, 'click', '.zoom-in', "Cannot click on 'zoom-in' button");
    driver.findElement({css:'div[id="workflow-canvas"]'}).getCssValue("transform").then(function (state) {b == state;})
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
    equal(a, b, 'zoom-in');

    basic.execute(driver, 'click', '.zoom-default', "Cannot click on 'zoom-default' button");
    driver.findElement({css:'div[id="workflow-canvas"]'}).getCssValue("transform").then(function (state) {a == state;})
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
    equal(a, b, 'zoom-default');

    basic.execute(driver, 'click', 'button[id="full-width"]', "Cannot click on 'full-width' button");
    driver.findElement({css:'div[id="props-col"]'}).isDisplayed().then(function (state) {
        if (state === true) {
            console.trace("Seems 'button[id=full-width]' does not work");
            process.exit(1);
        }
    });

    basic.execute(driver, 'click', '#workflow-save-button', "Cannot click on 'save net' button");
    driver.sleep(basic.SLOW_OPERATION);
    driver.findElements({css:'h4[about="v-fc:ChooseType"]'}).then(function(elements_arr) {
        if (elements_arr.length > 0) {
            console.trace("Seems save-button does not work");
            process.exit(1);
        }
    });

    driver.quit();
});
