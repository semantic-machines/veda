var basic = require('./basic.js'),
    timeStamp = ''+Math.round(+new Date()/1000),
    assert = require('assert');


function findUp(driver) {
    return driver.findElements({css:'a[property="rdfs:label"]'}).then(function (result) {
        return result[3];
    }).thenCatch(function (e) {basic.errorHandler(e, "Cannot find any task");});
}

function clickUp(element) {
    element.click()
        .thenCatch(function (e) {basic.errorHandler(e,"Cannot click on task");});
}

function decision(driver, number) {
    driver.findElement({css:'div[class="radio decision"] input[value="' + number + '"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on '" + number + "' decision");});
}

function welcome(driver) {
    driver.findElement({css:'a[href="#/v-l:Welcome"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Welcome' button")});
}


function open(driver) {
    driver.findElement({id:'menu'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button");});
    basic.isVisible(driver, 'li[id="menu"] li[resource="v-l:Inbox"]', basic.SLOW_OPERATION);
    driver.findElement({css:'li[id="menu"] li[resource="v-l:Inbox"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `inbox` button");});
    driver.sleep(1000);
}

function openMsg(driver, number, commentValue, chooseValue) {
    open(driver);
    driver.wait(findUp(driver), basic.FAST_OPERATION).then(clickUp);
    decision(driver, number);
    if (commentValue === '+') {
        driver.findElement({css:'veda-control[property="rdfs:comment"] div textarea'}).sendKeys(timeStamp)
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'comment'");});
    }
    if (chooseValue === '+') {
        driver.executeScript("document.querySelector('#fulltext').scrollIntoView(true)");
        basic.chooseFromDropdown(driver, 'v-wf:to', 'Администратор4', 'Администратор4 : Аналитик');
    }
    driver.sleep(basic.FAST_OPERATION);
    driver.executeScript("document.querySelector('#send').scrollIntoView(true)");
    driver.findElement({id:'send'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Ok' button");});
    welcome(driver);
}

function checkMsg(driver, count) {
    open(driver);
    driver.sleep(basic.FAST_OPERATION);
    driver.findElements({css:'span[property="v-s:description"]'}).then(function (result) {
        assert.equal(count, result.length);
    }).thenCatch(function (e) {basic.errorHandler(e, "Invalid `message` elements count");});
    welcome(driver);
}

module.exports = {
    checkTask: function (driver, count, login, password, firstName, lastName) {
        basic.login(driver, login, password, firstName, lastName);
        checkMsg(driver, count);
        basic.logout(driver);
    },

    acceptTask: function (driver, decision, commentValue, chooseValue, login, password, firstName, lastName) {
        basic.login(driver, login, password, firstName, lastName);
        openMsg(driver, decision, commentValue, chooseValue);
        driver.sleep(basic.FAST_OPERATION);
        basic.logout(driver);
    },

    checkRouteStatus: function (driver, element, color, count) {
        basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
        basic.openFulltextSearchDocumentForm(driver, 'Стартовая форма сети Комплексный маршрут', 's-wf:ComplexRouteStartForm');
        driver.findElement({id:'submit'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Submit/Отправить' button");});
        basic.isVisible(driver, 'span[rel="v-wf:isProcess"]', basic.FAST_OPERATION);
        driver.findElement({css:'span[rel="v-wf:isProcess"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'экземпляр маршрута: Комплексный маршрут' button");});
        driver.sleep(basic.FAST_OPERATION);
        driver.findElement({css:'.glyphicon-share-alt'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'glyphicon-share-alt'");});
        for (var i = 0; i < element.length; i++) {
            driver.findElements({css:'div[id="'+ element[i] +'"][colored-to="'+ color[i] +'"]'}).then(function (result) {
                assert.equal(count, result.length);
            }).thenCatch(function (e) {basic.errorHandler(e, "Seems route status is wrong");});
        }
        welcome(driver);
        basic.logout(driver);
    }

}



