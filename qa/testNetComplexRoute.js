var basic = require('./basic.js'),
    timeStamp = ''+Math.round(+new Date()/1000),
    assert = require('assert');

function openMsg(driver, decision) {
    driver.findElement({id:'menu'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button");});
    basic.isVisible(driver, 'li[id="menu"] li[resource="v-l:Inbox"]', basic.SLOW_OPERATION);
    driver.findElement({css:'li[id="menu"] li[resource="v-l:Inbox"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `inbox` button");});
    var container = driver.findElement({id:'main'});
    var content = container.innerHTML;
    container.innerHTML = content;
    driver.findElement({css:'a[property="rdfs:label"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Согласовать' button");});
    if (decision === '0') {
        driver.findElement({css:'div[class="radio decision"] input[value="0"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on '0' decision");});
    }
    if (decision === '1') {
        driver.findElement({css:'div[class="radio decision"] input[value="1"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on '1' decision");});
        driver.findElement({css:'veda-control[property="rdfs:comment"] div textarea'}).sendKeys(timeStamp)
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'comment'");});
    }
    if (decision === '2') {
        driver.findElement({css:'div[class="radio decision"] input[value="2"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on '2' decision");});
        driver.findElement({css:'veda-control[property="rdfs:comment"] div textarea'}).sendKeys(timeStamp)
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'comment'");});
    }
    if (decision === '3') {
        driver.findElement({css:'div[class="radio decision"] input[value="3"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on '3' decision");});
        driver.executeScript("document.querySelector('#fulltext').scrollIntoView(true)");
        basic.chooseFromDropdown(driver, 'v-wf:to', 'Администратор2', 'Администратор4 : Аналитик');
        driver.findElement({css:'veda-control[property="rdfs:comment"] div textarea'}).sendKeys(timeStamp)
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'comment'");});
    }
    if (decision === '4') {
        driver.findElement({css:'div[class="radio decision"] input[value="4"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on '4' decision");});
        driver.findElement({css:'veda-control[property="rdfs:comment"] div textarea'}).sendKeys(timeStamp)
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'comment'");});
    }

    driver.sleep(basic.FAST_OPERATION);
    driver.executeScript("document.querySelector('#send').scrollIntoView(true)");
    driver.findElement({id:'send'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Send' button");});
    driver.findElement({css:'a[href="#/v-l:Welcome"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Welcome' button")});
}

function checkMsg(driver, count) {
    driver.findElement({id:'menu'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button");});
    basic.isVisible(driver, 'li[id="menu"] li[resource="v-l:Inbox"]', basic.SLOW_OPERATION);
    driver.findElement({css:'li[id="menu"] li[resource="v-l:Inbox"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `inbox` button");});
    var container = driver.findElement({id:'main'});
    var content = container.innerHTML;
    container.innerHTML = content;
    driver.sleep(basic.FAST_OPERATION);
    driver.findElements({css:'a[property="rdfs:label"]'}).then(function (result) {
        assert.equal(count, result.length);
    }).thenCatch(function (e) {basic.errorHandler(e, "Invalid `message` elements count");});
    driver.findElement({css:'a[href="#/v-l:Welcome"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Welcome' button")});
}


function checkTask(driver, count, login, password, firstName, lastName) {
    basic.login(driver, login, password, firstName, lastName);
    checkMsg(driver, count);
    basic.logout(driver);
}

function acceptTask(driver, decision, login, password, firstName, lastName) {
    basic.login(driver, login, password, firstName, lastName);
    openMsg(driver, decision);
    driver.sleep(basic.FAST_OPERATION);
    basic.logout(driver);
}

function checkRouteStatus(driver, element, color) {
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    basic.openFulltextSearchDocumentForm(driver, 'Стартовая форма сети Комплексный маршрут', 's-wf:ComplexRouteStartForm');
    driver.findElement({id:'submit'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'submit' button");});
    driver.findElement({css:'span[rel="v-wf:isProcess"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'экземпляр маршрута :Комплексный маршрут' button");});
    driver.sleep(basic.FAST_OPERATION);
    driver.findElement({css:'.glyphicon-share-alt'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'glyphicon-share-alt'");});
    for (var i = 0; i < element.length; i++) {
        driver.findElement({css:'div[id="'+ element[i] +'"][colored-to="'+ color[i] +'"]'})
            .thenCatch(function (e) {basic.errorHandler(e, "Seems " + element[i] + " is not" + color[i] + "/routeStatus is wrong");});
    }
    driver.findElement({css:'a[href="#/v-l:Welcome"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Welcome' button")});
    basic.logout(driver);
}



basic.getDrivers().forEach (function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    basic.openCreateDocumentForm(driver, 'Тестовый шаблон комплексного маршурута', 's-wf:ComplexRouteTest');
    driver.executeScript("document.querySelector('#send').scrollIntoView(true)");
    driver.findElement({id:'send'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Send' button");});

    driver.executeScript("document.querySelector('#save_and_start_process').scrollIntoView(true)");
    driver.sleep(basic.FAST_OPERATION);
    driver.findElement({id:'save_and_start_process'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'save_and_start_process' button");});
    driver.sleep(basic.FAST_OPERATION);

    //coordination1

    openMsg(driver, '0');
    driver.sleep(3000);
    basic.logout(driver);

    acceptTask(driver, '0', 'bychinat', '123', '4', 'Администратор4');
    checkRouteStatus(driver, ['s-wf:cr_c1', 's-wf:cr_c2'], ['green', 'red']);

    //coordination2

    acceptTask(driver, '0', 'karpovrt', '123', '2', 'Администратор2');
    acceptTask(driver, '1', 'bychinat', '123', '4', 'Администратор4');
    checkRouteStatus(driver, ['s-wf:cr_c2', 's-wf:cr_rework'], ['green', 'red']);
    acceptTask(driver, '1', 'karpovrt', '123', '2', 'Администратор2');
    acceptTask(driver, '0', 'bychinat', '123', '4', 'Администратор4');
    acceptTask(driver, '0', 'karpovrt', '123', '2', 'Администратор2');
    checkRouteStatus(driver, ['s-wf:cr_review', 's-wf:cr_instruction', 's-wf:cr_examination'],
        ['red', 'red', 'red']);

    //review, instruction, examination -> instruction2
    checkTask(driver, '3', 'bychinat', '123', '4', 'Администратор4');
    checkTask(driver, '0', 'karpovrt', '123', '2', 'Администратор2');
    acceptTask(driver, '0', 'bychinat', '123', '4', 'Администратор4');
    checkRouteStatus(driver, ['s-wf:cr_review', 's-wf:cr_instruction', 's-wf:cr_examination', 's-wf:cr_instruction2'],
        ['red', 'red', 'green', 'red']);
    checkTask(driver, '0', 'karpovrt', '123', '2', 'Администратор2');
    acceptTask(driver, '0', 'bychinat', '123', '4', 'Администратор4');
    checkRouteStatus(driver, ['s-wf:cr_review', 's-wf:cr_instruction', 's-wf:cr_examination', 's-wf:cr_instruction2'],
        ['green', 'red', 'green', 'red']);
    checkTask(driver, '0', 'karpovrt', '123', '2', 'Администратор2');
    acceptTask(driver, '0', 'bychinat', '123', '4', 'Администратор4');
    checkRouteStatus(driver, ['s-wf:cr_review', 's-wf:cr_instruction', 's-wf:cr_examination', 's-wf:cr_instruction2'],
        ['green', 'green', 'green', 'red']);

    checkTask(driver, '1', 'karpovrt', '123', '2', 'Администратор2');
    acceptTask(driver, '3', 'karpovrt', '123', '2', 'Администратор2');
    acceptTask(driver, '4', 'bychinat', '123', '4', 'Администратор4');
    acceptTask(driver, '1', 'karpovrt', '123', '2', 'Администратор2');
    acceptTask(driver, '2', 'bychinat', '123', '4', 'Администратор4');
    acceptTask(driver, '1', 'karpovrt', '123', '2', 'Администратор2');
    acceptTask(driver, '0', 'bychinat', '123', '4', 'Администратор4');

    checkRouteStatus(driver, ['s-wf:cr_instruction2', 's-wf:cr_finish'],
        ['red' , 'red']);

    driver.quit();
});

