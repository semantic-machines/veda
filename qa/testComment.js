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

/**
 * Проверка кнопок Комментировать, Ответить, Редактировать, Удалить
 * @param driver
 * @param comment - количество кнопок Комментировать, которое должно быть;
 * @param reply - количество кнопок Ответить, которое должно быть;
 * @param edit - количество кнопок Редактировать, которое должно быть;
 * @param del - количество кнопок Удалить, которое должно быть;
*/


function check(driver, comment, reply, edit, del) {
    driver.executeScript("document.querySelector('#comment-content').scrollIntoView(true);");
    driver.findElements({css:'#comment-content'}).then(function (result) {
        assert.equal(comment, result.length);
    }).thenCatch(function (e) {basic.errorHandler(e, "Seems number of 'comments' is wrong, expected: " + comment);});
    driver.findElements({css:'#reply'}).then(function (sum) {
        driver.findElements({css: 'a[id="reply"][style="display: none;"]'}).then(function (visible) {
            assert.equal(reply, sum.length - visible.length);
        }).thenCatch(function (e) {basic.errorHandler(e, "Seems number of 'reply' buttons is wrong, expected: " + reply);});
    });
    driver.findElements({css:'#edit-comment'}).then(function (sum) {
        driver.findElements({css:'a[id="edit-comment"][style="display: none;"]'}).then(function (visible) {
            assert.equal(edit, sum.length - visible.length);
        }).thenCatch(function (e) {basic.errorHandler(e, "Seems number of 'edit-comment' buttons is wrong, expected: " + edit);});
    });
    driver.findElements({css:'#delete'}).then(function (sum) {
        driver.findElements({css:'a[id="delete"][style="display: none;"]'}).then(function (visible) {
            assert.equal(del, sum.length - visible.length);
        }).thenCatch(function (e) {basic.errorHandler(e, "Seems number of 'delete' buttons is wrong, expected: " + del);});
    });
}

/**
 * Создание комментария с указаным текстом
 * @param driver 
 * @param somethingUnique - текст, который будет в комментарии
*/

function comment(driver, somethingUnique) {
    driver.executeScript("document.querySelector('em[about=\"rdfs:comment\"').scrollIntoView(true);");
    basic.execute(driver, 'sendKeys', 'textarea[class="form-control"]', "Cannot input comment", somethingUnique);
    driver.executeScript("document.querySelector('div[typeof=\"v-s:Comment\"] button[id=\"save\"]').scrollIntoView(true);");
    basic.execute(driver, 'click', 'div[typeof="v-s:Comment"] button[id="save"]', "Cannot click  on 'save' button");
    driver.sleep(basic.SLOW_OPERATION/2);
    driver.findElement({css:'div[id="comment-content"]'}).thenCatch(function (e) {basic.errorHandler(e, "Cannot find new comment");});
}

/**
 * 1.Open page -> Login(as kaprovrt);
 * 2.Open Administrator2 profile -> Add comment -> Reply comment;
 * 3.Check number of buttons -> Delet reply -> Check number of buttons;
 * 4.Logout -> Login(as bychinat) -> Check number of buttons;
 * 5.Quit;
 *
 * 1.Открываем страницу -> Заходим в систему под karpovrt;
 * 2.Заходим в профиль Администратор2 -> Добавляем комментарий1 -> Отвечаем на комментарий1;
 * 3.Проверяем количество кнопок -> Удаляем ответ -> Проверяем количество кнопок;
 * 4.Выходим из системы -> Заходим в систему под bychinat -> Проверяем количество кнопок;
 * 5.Выход;
*/

basic.getDrivers().forEach(function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    basic.execute(driver, 'click', '#user-info', "Cannot click on 'user-info' button");
    driver.executeScript("document.querySelector('#add-comment').scrollIntoView(true);");
    basic.execute(driver, 'click', '#add-comment', "Cannot click on 'add-comment' button");
    comment(driver, timeStamp);
    driver.executeScript("document.querySelector('#reply').scrollIntoView(true);");
    basic.execute(driver, 'click', '#reply', "Cannot click on 'reply' button");
    comment(driver, timeStamp + 1);

    check(driver, 2, 2, 1, 1);
    driver.executeScript("document.querySelector('#delete').scrollIntoView(true);");
    driver.wait(findUp(driver), basic.FAST_OPERATION).then(clickUp);
    driver.switchTo().alert().accept();
    driver.sleep(basic.SLOW_OPERATION/2);
    check(driver, 1, 1, 1, 1);

    basic.logout(driver);
    basic.login(driver, 'bychinat', '123', '4', 'Администратор4');
    driver.navigate().refresh();
    driver.sleep(basic.SLOW_OPERATION);
    driver.executeScript("document.querySelector('#reply').scrollIntoView(true);");
    check(driver, 1, 1, 0, 0);

    driver.quit();
});