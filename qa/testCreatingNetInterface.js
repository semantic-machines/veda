var basic = require('./basic.js'),
    webdriver = require('selenium-webdriver');

/**
 * Проверка нахождения элемента
 * @param driver
 * @param element - элемент
 * @param phase - текущая фаза теста
 */

function check(driver, element, phase) {
    driver.findElements({css:''+element}).then(function(elements_arr){
        if (elements_arr.length > 0) {
            console.trace("****** PHASE#" + phase + " : ERROR = Seems "+ element +" has not deleted");
            process.exit(1);
        }
    }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Cannot find any "+ element);});
}

/**
 * Удаление текущего элемента
 * @param driver
 * @param phase - текущая фаза теста
*/
function deleteElement(driver, phase) {
    basic.execute(driver, 'click', '.delete-state', "****** PHASE#" + phase + " : ERROR = Cannot click on 'delete' button");
    driver.switchTo().alert().accept();
}

/**
 * Проверка состояни до и после нажатия кнопки
 * @param a - состояние до нажатия кнопки
 * @param b - состояние после нажатия кнопки
 * @param button - кнопка
 * @param phase - текущая фаза теста
*/
function equal(a, b, button, phase) {
    if(a == b) {
        console.trace("****** PHASE#" + phase + " : ERROR = Seems " + button + " does not work");
        process.exit(1);
    }
}

/**
 * 0.Open page -> login(as karpovrt);
 * 1.Open create net document form;
 * 2.Create flow from input to output -> Delete flow -> Check flow is no on canvas;
 * 3.Create task1 -> Copy task1(task2) -> Delete task1 -> Delete task2 -> Check, tasks are no on canvas;
 * 4.Create condition -> Check condition;
 * 5.Default state(state1) -> Zoom-out(state2) -> Equal state1 & state2;
 * 6.Zoom-in -> Zoom-in(state3) -> Equal state1 & state3;
 * 7.Zoom-default(state4) -> Equal state3 & state4;
 * 8.Full-width -> Check canvas in full-width;
 * 9.Save net;
 *
 * 0.Открываем страницу -> Входим в систему под karpovrt;
 * 1.Открываем форму создания сети;
 * 2.Соединяем стрелкой вход и выход -> удаляем стрелку -> Проверяем удалена ли она;
 * 3.Создаем задачу1 -> Копируем ее(задача2) -> Удаляем задачу1 -> Удаляем задачу2 -> Проверяем, что задач нет;
 * 4.Создаем состояние -> Проверяем создалось ли оно;
 * 5.Обычное состояние(состояние1) -> Уменьшаем масштаб(состояние2) -> Проверяем, что они разные;
 * 6.Увеличиваем масштаб -> Увеличиваем масштаб(состояние3) -> Проверяем, что состояние1 и состояние3 разные;
 * 7.Обычное состояние(состояние4) -> Проверяем, что состояние4 и состояние3 разные;
 * 8.Проверяем, что после нажатия, редактор стал во всю ширину.
 * 9.Сохраняем сеть;
*/


basic.getDrivers().forEach(function (drv) {
    //PHASE#0: Login
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 0);

    //PHASE#1: Open form
    basic.openCreateDocumentForm(driver, 'Сеть', 'v-wf:Net', 1);
    basic.isVisible(driver, '.workflow-canvas-wrapper', basic.FAST_OPERATION * 2, 1);
    // basic.execute(driver, 'click', '.workflow-canvas-wrapper', "Cannot click on net canvas");
    var startPoint = driver.findElement({css:'.glyphicon-play'});
    var actionSequence = webdriver.ActionSequence;        
    var act = new actionSequence(driver);
    act.mouseMove(startPoint, {x: 200, y: 0}).click().perform();
    basic.isVisible(driver, 'span[about="v-wf:Net"]', basic.FAST_OPERATION * 2, 1);

    //PHASE#2: Flow
    new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-io-condition-input .ep'}),
        driver.findElement({css:'.glyphicon-stop'})).perform();
    basic.execute(driver, 'click', 'svg[class="_jsPlumb_connector"]', "****** PHASE#2 > Flow : ERROR = Cannot click on 'connector' arrow");
    deleteElement(driver, 2);
    check(driver, 'svg[class="_jsPlumb_connector"]', 2);

    //PHASE#3: Task
    basic.execute(driver, 'click', '.create-task', "****** PHASE#3 > Task : ERROR = Cannot click on 'create-task' button");
    basic.execute(driver, 'click', '.state-task', "****** PHASE#3 > Task : ERROR = Cannot click on 'state-task' button");
    basic.execute(driver, 'click', '.copy-net-element', "****** PHASE#3 > Task : ERROR = Cannot click on 'copy-net-element' button");
    deleteElement(driver, 3);
    basic.execute(driver, 'click', '.state-task', "****** PHASE#3 > Task : ERROR = Cannot click on 'state-task' button");
    deleteElement(driver, 3);
    check(driver, '.state-task', 3);

    //PHASE#4: Condition
    basic.execute(driver, 'click', '.create-condition', "****** PHASE#4 > Condition : ERROR = Cannot click 'create-condition' button");
    basic.execute(driver, 'click', '.state-condition', "****** PHASE#4 > Condition : ERROR = Cannot click 'state-condition' button");

    //PHASE#5: Zoom-out
    var a = " ", b = "";
    driver.findElement({css:'div[id="workflow-canvas"]'}).getCssValue("transform").then(function (state) {a = state;})
        .thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#5 > Zoom-out : ERROR = Cannot click on net canvas");});
    basic.execute(driver, 'click', '.zoom-out', "Cannot click on 'zoom-out' button");
    driver.findElement({css:'div[id="workflow-canvas"]'}).getCssValue("transform").then(function (state) {b == state;})
        .thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#5 > Zoom-out : ERROR = Cannot click on net canvas");});
    equal(a, b, 'zoom-out', 5);

    //PHASE#6: Zoom-in
    basic.execute(driver, 'click', '.zoom-in', "****** PHASE#6 > Zoom-in : ERROR = Cannot click on 'zoom-in' button");
    basic.execute(driver, 'click', '.zoom-in', "****** PHASE#6 > Zoom-in : ERROR = Cannot click on 'zoom-in' button");
    driver.findElement({css:'div[id="workflow-canvas"]'}).getCssValue("transform").then(function (state) {b == state;})
        .thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#6 > Zoom-in : ERROR = Cannot click on net canvas");});
    equal(a, b, 'zoom-in', 6)

    //PHASE#7: Zoom-default
    basic.execute(driver, 'click', '.zoom-default', "****** PHASE#7 > Zoom-default : ERROR = Cannot click on 'zoom-default' button");
    driver.findElement({css:'div[id="workflow-canvas"]'}).getCssValue("transform").then(function (state) {a == state;})
        .thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#7 > Zoom-default : ERROR = Cannot click on net canvas");});
    equal(a, b, 'zoom-default', 7);

    //PHASE#8: Full-width
    basic.execute(driver, 'click', 'button[id="full-width"]', "****** PHASE#8 > Full-width : ERROR = Cannot click on 'full-width' button");
    driver.findElement({css:'div[id="props-col"]'}).isDisplayed().then(function (state) {
        if (state === true) {
            console.trace("****** PHASE#8 > Full-width : ERROR = Seems 'button[id=full-width]' does not work");
            process.exit(1);
        }
    });

    //PHASE#9: Save
    basic.execute(driver, 'click', '#workflow-save-button', "****** PHASE#9 > Save : ERROR = Cannot click on 'save net' button");
    driver.quit();
});
