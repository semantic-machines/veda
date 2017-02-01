var basic = require('./basic.js'),
    timeStamp = ''+Math.round(+new Date()/1000),
    assert = require('assert');

function findUp(driver) {
    return driver.findElements({css:'#delete'}).then(function (result) {
        return result[1];
    }).thenCatch(function(e){basic.errorHandler(e, "Cannot find delete buttons");});
}

function clickUp(element) {
    element.click()
        .thenCatch(function (e) {basic.errorHandler(e,"Cannot click on delete button");});
}

function check(driver, comment, reply, edit, del) {
    driver.executeScript("document.querySelector('#comment-content').scrollIntoView(true);");
    var a;
    driver.findElements({css:'#comment-content'}).then(function (result) {
        assert.equal(comment, result.length);
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems number of 'comments' is wrong, expected: " + comment);})

    driver.findElements({css:'#reply'}).then(function (result) {
        a = result.length;
    })
    driver.findElements({css:'a[id="reply"][style="display: none;"]'}).then(function (result) {
        assert.equal(reply, a - result.length);
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems number of 'reply' buttons is wrong, expected: " + reply);})

    driver.findElements({css:'#edit-comment'}).then(function (result) {
        a = result.length;
    })
    driver.findElements({css:'a[id="edit-comment"][style="display: none;"]'}).then(function (result) {
        assert.equal(edit, a - result.length);
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems number of 'edit-comment' buttons is wrong, expected: " + edit);})

    driver.findElements({css:'#delete'}).then(function (result) {
        a = result.length;
    })
    driver.findElements({css:'a[id="delete"][style="display: none;"]'}).then(function (result) {
        assert.equal(del, a - result.length);
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems number of 'delete' buttons is wrong, expected: " + del);})
}

function comment(driver, somethingUnique) {
    driver.executeScript("document.querySelector('em[about=\"rdfs:comment\"').scrollIntoView(true);");
    driver.findElement({css:'textarea[class="form-control"]'}).sendKeys(somethingUnique)
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot input comment");});
    driver.executeScript("document.querySelector('div[typeof=\"v-s:Comment\"] button[id=\"save\"]').scrollIntoView(true);");
    driver.findElement({css:'div[typeof="v-s:Comment"] button[id="save"]'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click  on 'save' button");});
    driver.sleep(basic.FAST_OPERATION);
    driver.executeScript("location.reload();");
    driver.sleep(basic.SLOW_OPERATION);
    //basic.isVisible(driver, 'div[id="comment-content"]', basic.SLOW_OPERATION);
    driver.findElement({css:'div[id="comment-content"]'})
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot find new comment");});
}

basic.getDrivers().forEach(function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);

    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
    driver.findElement({css:'#user-info'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'user-info' button");});

    driver.executeScript("document.querySelector('#add-comment').scrollIntoView(true);");
    driver.findElement({css:'#add-comment'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'add-comment' button");});
    comment(driver, timeStamp);

    driver.executeScript("document.querySelector('#reply').scrollIntoView(true);");
    driver.findElement({css:'#reply'}).click()
        .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'reply' button");});
    comment(driver, timeStamp + 1);

    check(driver, 2, 2, 1, 1);
    driver.executeScript("document.querySelector('#delete').scrollIntoView(true);");
    driver.wait(findUp(driver), basic.FAST_OPERATION).then(clickUp);
    driver.switchTo().alert().accept();
    driver.executeScript("location.reload();");
    driver.sleep(basic.SLOW_OPERATION);
    check(driver, 1, 1, 1, 1);

    basic.logout(driver);
    basic.login(driver, 'bychinat', '123', '4', 'Администратор4');
    driver.executeScript("document.querySelector('#reply').scrollIntoView(true);");
    check(driver, 1, 1, 0, 0);
    driver.sleep(basic.FAST_OPERATION);

    driver.quit();
});