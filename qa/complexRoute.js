var basic = require('./basic.js'),
    timeStamp = ''+Math.round(+new Date()/1000),
    assert = require('assert');

function welcome(driver) {
    driver.executeScript("document.querySelector('.navbar-brand').scrollIntoView(true)");
    driver.sleep(basic.FAST_OPERATION * 2);
    basic.execute(driver, 'click', 'a[href="#/v-l:Welcome"]', "Cannot click on 'Welcome' button");
}

module.exports = {
    /**
     * Проверка сообщений
     * @param driver
     * @param count - количество сообщений
     * @param login       |
     * @param password    | Данные для входа
     * @param firstName   |
     * @param lastName    |
    */
    checkTask: function (driver, count, login, password, firstName, lastName, phase) {
        basic.login(driver, login, password, firstName, lastName, phase);
        basic.menu(driver, 'Inbox', phase);
        driver.sleep(basic.SLOW_OPERATION);
        driver.findElements({css:'tr[typeof="v-wf:DecisionForm s-wf:UserTaskForm"]'}).then(function (result) {
            assert.equal(count, result.length);
        }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = checkTask:Invalid `message` elements count (inbox task counter), user=" + login + ':' + firstName + ':' + lastName);});
        welcome(driver);
        basic.logout(driver, phase);
    },

    /**
     * Ответ на сообщение
     * @param driver
     * @param decision - номер решения
     * @param commentValue - статус комментирования
     * @param chooseValue - статут выбора Персоны
     * @param login       |
     * @param password    | Данные для входа
     * @param firstName   |
     * @param lastName    |
     * @param phase - текущая фаза теста
    */
    acceptTask: function (driver, decision, commentValue, chooseValue, login, password, firstName, lastName, phase) {
        basic.login(driver, login, password, firstName, lastName, phase);
        basic.menu(driver, 'Inbox', phase);
        driver.sleep(basic.SLOW_OPERATION);
        driver.wait(basic.findUp(driver, 'a[property="rdfs:label"]', 3, "****** PHASE#" + phase + " : ERROR = Cannot find 'rdfs:label'"), basic.FAST_OPERATION).then(
            function(result){basic.clickUp(result);});
        basic.execute(driver, 'click', 'div[class="radio decision"] input[value="' + decision + '"]', "****** PHASE#" + phase + " : ERROR = Cannot click on '" + decision + "' decision");
        if (commentValue === '+') {
            basic.execute(driver, 'sendKeys', 'veda-control[property="rdfs:comment"] div textarea', "****** PHASE#" + phase + " : ERROR = Cannot fill 'comment'", timeStamp);
        }
        if (chooseValue === '+') {
            driver.executeScript("document.querySelector('#fulltext').scrollIntoView(true)");
            basic.chooseFromDropdown(driver, 'v-wf:to', 'Администратор4', 'Администратор4 : Аналитик', phase);
        }
        driver.sleep(basic.FAST_OPERATION);
        driver.executeScript("document.querySelector('#send').scrollIntoView(true)");
        basic.execute(driver, 'click', 'button[id="send"]', "****** PHASE#" + phase + " : ERROR = Cannot click on 'Ok' button");
        welcome(driver);
        basic.logout(driver, phase);
    },
    /**
     * Проверка статуса маршрута
     * @param driver
     * @param element - список элементов
     * @param color - список цветов элементов
     * @param count - количество элементов в данном состоянии
     * @param docNumber - номер документа в поиске
    */

    checkRouteStatus: function (driver, element, color, count, docNumber) {
        basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
        basic.openFulltextSearchDocumentForm(driver, 'Стартовая форма сети Комплексный маршрут', 's-wf:ComplexRouteStartForm');
        basic.execute(driver, 'click', 'button[id="submit"]', "Cannot click on 'Submit/Отправить' button");
        driver.sleep(basic.SLOW_OPERATION);
        driver.wait(basic.findUp(driver, 'span[rel="v-wf:isProcess"]', docNumber, "Cannot find isProcess"),
            basic.FAST_OPERATION).then(function(result) {basic.clickUp(result);});
        driver.sleep(basic.FAST_OPERATION);
        basic.execute(driver, 'click', '.glyphicon-share-alt', "Cannot click on 'glyphicon-share-alt'");
        for (var i = 0; i < element.length; i++) {
            driver.findElements({css:'div[id="'+ element[i] +'"][colored-to="'+ color[i] +'"]'}).then(function (result) {
                assert.equal(count, result.length);
            }).thenCatch(function (e) {basic.errorHandler(e, "Seems route status is wrong");});
        }
        welcome(driver);
        basic.logout(driver);
    },

    /**
     * Проверка счетчиков задач
     * @param driver
     * @param inbox - счетчик входящих
     * @param outbox - счетчик исходящих
     * @param completed - счетчик выполненных
     * @param login       |
     * @param password    | Данные для входа
     * @param firstName   |
     * @param lastName    |
     */
    checkTasks: function (driver, inbox, outbox, completed, login, password, firstName, lastName) {
        basic.login(driver, login, password, firstName, lastName);
        basic.menu(driver, 'Inbox');
        driver.sleep(basic.SLOW_OPERATION);
        driver.findElement({css:'a[about="v-ft:Inbox2"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e,"Cannot click on Inbox messages")});
        driver.findElement({css:'span[property="v-ft:inboxCount"]'}).getText().then(function (result) {
            assert.equal(inbox, result);
        }).thenCatch(function (e) {basic.errorHandler(e, "Seems number of inbox messages is wrong: expected = " + inbox);});
        driver.findElement({css:'a[about="v-ft:Outbox2"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e,"Cannot click on Outbox messages")});
        driver.findElement({css:'span[property="v-ft:outboxCount"]'}).getText().then(function (result) {
            assert.equal(inbox, result);
        }).thenCatch(function (e) {basic.errorHandler(e, "Seems number of inbox messages is wrong: expected = " + inbox);});
        driver.findElement({css:'a[about="v-ft:Completed2"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e,"Cannot click on Completed messages")});
        driver.findElement({css:'span[property="v-ft:completedCount"]'}).getText().then(function (result) {
            assert.equal(inbox, result);
        }).thenCatch(function (e) {basic.errorHandler(e, "Seems number of inbox messages is wrong: expected = " + inbox);});
        welcome(driver);
        basic.logout(driver);
    }

};



