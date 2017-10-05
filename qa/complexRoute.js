var basic = require('./basic.js'),
    timeStamp = ''+Math.round(+new Date()/1000),
    assert = require('assert');

function welcome(driver, phase) {
    driver.executeScript("document.querySelector('.navbar-brand').scrollIntoView(true)")
        .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Cannot scroll to welcome bar");});
    driver.sleep(basic.FAST_OPERATION * 2);
    basic.execute(driver, 'click', 'a[href="#/v-l:Welcome"]', "****** PHASE#" + phase + " : ERROR = Cannot click on 'Welcome' button");
}

module.exports = {
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
    acceptTask: function (driver, decision, commentValue, chooseValue, login, password, firstName, lastName, phase, to1, to2) {
        basic.login(driver, login, password, firstName, lastName, phase);
        basic.menu(driver, 'Inbox', phase);
        driver.sleep(basic.FAST_OPERATION*2);
//  ------ без этого кода тесты часто не проходят ! ???? 
        driver.findElement({css:'a[about="v-ft:Inbox"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e,"****** PHASE#" + phase + " : ERROR = Cannot click on Inbox messages")});
        driver.sleep(basic.FAST_OPERATION);
//  ------
        driver.wait(basic.findUp(driver, 'a[property="rdfs:label"]', 3, "****** PHASE#" + phase + " : ERROR = Cannot find 'rdfs:label'"), basic.FAST_OPERATION*2).then(
            function(result){basic.clickUp(result, "****** PHASE#" + phase + " : ERROR = Cannot click on message. Seems message is not located");});
        basic.execute(driver, 'click', 'div[class="radio decision"] input[value="' + decision + '"]', "****** PHASE#" + phase + " : ERROR = Cannot click on '" + decision + "' decision");
        driver.sleep(basic.FAST_OPERATION);
        if (commentValue === '+') {
            basic.execute(driver, 'sendKeys', 'veda-control[property="rdfs:comment"] div textarea', "****** PHASE#" + phase + " : ERROR = Cannot fill 'comment'", timeStamp);
        }
        driver.sleep(basic.FAST_OPERATION);
        if (chooseValue === '+') {
            driver.executeScript("document.querySelector('#fulltext').scrollIntoView(true)")
                .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Cannot scroll to fulltext button");});
            basic.chooseFromDropdown(driver, 'v-wf:to', to1, to2, phase);
        }
        driver.sleep(basic.FAST_OPERATION);
        driver.executeScript("document.querySelector('#send').scrollIntoView(true)")
            .thenCatch(function(e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Cannot scroll to send button");});
        basic.execute(driver, 'click', 'button[id="send"]', "****** PHASE#" + phase + " : ERROR = Cannot click on 'Ok' button");
        welcome(driver, phase);
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
        basic.openFulltextSearchDocumentForm(driver, 'Стартовая форма сети Комплексный маршрут', 's-wf:ComplexRouteStartForm');
        basic.execute(driver, 'click', 'button[id="submit"]', "Cannot click on 'Submit/Отправить' button");
        driver.sleep(basic.SLOW_OPERATION);
        driver.wait(basic.findUp(driver, 'span[rel="v-wf:isProcess"]', docNumber, "Cannot find isProcess"),
            basic.FAST_OPERATION).then(function(result) {basic.clickUp(result, "****** PHASE# : ERROR = Cannot click on isProcess");});
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
        welcome(driver, phase);
        basic.logout(driver, phase);
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
    checkTasks: function (driver, inbox, outbox, completed, login, password, firstName, lastName, phase) {
        basic.login(driver, login, password, firstName, lastName);
        basic.menu(driver, 'Inbox');
        driver.sleep(basic.FAST_OPERATION * 2);
        driver.findElement({css:'a[about="v-ft:Inbox"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e,"****** PHASE#" + phase + " : ERROR = Cannot click on Inbox messages")});
        driver.sleep(basic.FAST_OPERATION);
        driver.findElement({css:'span[property="v-ft:inboxCount"]'}).getText().then(function (result) {
            assert.equal(inbox, result);
        }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Seems number of inbox messages is wrong: expected = " + inbox);});
        driver.findElements({css:'a[property="rdfs:label"]'}).then(function(result) {
            assert.equal(inbox, result.length - 3);
        }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Seems number of displayed inbox messages in is wrong: expected = " + inbox);});
        driver.findElement({css:'a[about="v-ft:Outbox"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e,"Cannot click on Outbox messages")});
        driver.sleep(basic.FAST_OPERATION);    
        driver.findElement({css:'span[property="v-ft:outboxCount"]'}).getText().then(function (result) {
            assert.equal(outbox, result);
        }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Seems number of outbox messages is wrong: expected = " + outbox);});
        driver.findElements({css:'a[property="rdfs:label"]'}).then(function(result) {
            assert.equal(outbox, result.length - 3);
        }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Seems number of displayed outbox messages in is wrong: expected = " + outbox);});
        driver.findElement({css:'a[about="v-ft:Completed"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e,"****** PHASE#" + phase + " : ERROR = Cannot click on Completed messages")});
        driver.sleep(basic.FAST_OPERATION);
        driver.findElement({css:'span[property="v-ft:completedCount"]'}).getText().then(function (result) {
            assert.equal(completed, result);
        }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Seems number of completed messages is wrong: expected = " + completed);});
        driver.findElements({css:'a[property="rdfs:label"]'}).then(function(result) {
            assert.equal(completed, result.length - 3);
        }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#" + phase + " : ERROR = Seems number of displayed completed messages in is wrong: expected = " + completed);});
        welcome(driver, phase);
        basic.logout(driver);
    }
};



