var basic = require('./basic.js'),
    timeStamp = ''+Math.round(+new Date()/1000),
    assert = require('assert');

function welcome(driver) {
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
    checkTask: function (driver, count, login, password, firstName, lastName) {
        driver.sleep(basic.SLOW_OPERATION);
        basic.login(driver, login, password, firstName, lastName);
        driver.findElement({css:'li[about="v-ft:Inbox2"] span[id="counter"]'}).getText().then(function (result) {
	    //if (result == '')
	    //	result = '0';
            assert.equal(count, result);
        }).thenCatch(function (e) {basic.errorHandler(e, "checkTask:Invalid `message` elements count");});
        welcome(driver);
        basic.logout(driver);
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
    */
    acceptTask: function (driver, decision, commentValue, chooseValue, login, password, firstName, lastName) {
        basic.login(driver, login, password, firstName, lastName);
        basic.menu(driver, 'Inbox');
        driver.sleep(basic.FAST_OPERATION);
        driver.wait(basic.findUp(driver, 'a[property="rdfs:label"]', 3, "Cannot find 'rdfs:label'"), basic.FAST_OPERATION).then(
            function(result){basic.clickUp(result);});
        basic.execute(driver, 'click', 'div[class="radio decision"] input[value="' + decision + '"]', "Cannot click on '" + decision + "' decision");
        if (commentValue === '+') {
            basic.execute(driver, 'sendKeys', 'veda-control[property="rdfs:comment"] div textarea', "Cannot fill 'comment'", timeStamp);
        }
        if (chooseValue === '+') {
            driver.executeScript("document.querySelector('#fulltext').scrollIntoView(true)");
            basic.chooseFromDropdown(driver, 'v-wf:to', 'Администратор4', 'Администратор4 : Аналитик');
        }
        driver.sleep(basic.FAST_OPERATION);
        driver.executeScript("document.querySelector('#send').scrollIntoView(true)");
        basic.execute(driver, 'click', 'button[id="send"]', "Cannot click on 'Ok' button");
        welcome(driver);
            basic.logout(driver);
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
    }

}



