var webdriver = require('selenium-webdriver'),
    FirefoxProfile = require('firefox-profile'),
    basic = require('./basic.js');
/**
 * For FF, IE, GC browsers:
 * 0.Open page -> login(as karpovrt);
 * 1.Download selected graph(file);
 * 2.Upload file -> Download file;
 *
 * 0.Открываем страницу -> Входим в систему под karpovrt;
 * 1.Скачиваем выбранный граф(файл);
 * 2.Загружаем файл на сервер;
 */


basic.getThreeDrivers().forEach(function (drv) {
    if (drv.browser == 'chrome') {
        //PHASE#0: Login
        var driver = basic.getDriver(drv);
        basic.openPage(driver, drv);
        basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 0);

        //PHASE#1: Download file
        basic.execute(driver, 'click', '#user-info', "****** PHASE#1 : ERROR = Cannot click on 'user-info' button");
        basic.execute(driver, 'click', 'a[href="#/td:RomanKarpov//v-ui:Graph"]', "****** PHASE#1 : ERROR = Cannot click on 'glyphicon-link'");
        driver.sleep(basic.FAST_OPERATION);
        basic.execute(driver, 'click', 'button[id="export-ttl"]', "****** PHASE#1 : ERROR = Cannot click on 'export-ttl' button");

        //PHASE#2: Upload file
        var filePath = 'C:/Users/Administrator/Downloads/exported_graph.ttl';
        //var filePath = 'C:/Users/zugzug/Downloads/exported_graph.ttl';
        basic.openCreateDocumentForm(driver, 'Отчет', 'v-s:Report', 2);
        driver.executeScript("document.querySelector('strong[about=\"v-s:attachment\"]').scrollIntoView(true);")
            .thenCatch(function(e){basic.errorHandler(e, "****** PHASE#2 : ERROR = Cannot scroll to 'v-s:attachment' field")});
        basic.execute(driver, 'sendKeys', 'input[type="file"]', "****** PHASE#2 : ERROR = Cannot find '" + filePath + "' file", filePath);
        driver.sleep(basic.FAST_OPERATION);
        basic.execute(driver, 'click', 'span[property="v-s:fileName"]', "****** PHASE#2 : ERROR = Cannot click on file name");
    }
    if (drv.browser == 'firefox') {
        profile = new FirefoxProfile();
        profile.setPreference("browser.download.folderList",2);
        profile.setPreference("browser.download.manager.showWhenStarting", false);
        //profile.setPreference("browser.download.dir", "C:\\Users\\Administrator\\Downloads\\");
        profile.setPreference("browser.download.dir", "C:\\Users\\zugzug\\Downloads\\");
        profile.setPreference("browser.helperApps.neverAsk.saveToDisk","text/turtle");
        profile.setPreference("browser.helperApps.neverAsk.openFile","text/turtle");
        profile.setPreference("browser.helperApps.alwaysAsk.force", false);
        profile.setPreference("browser.download.manager.showAlertOnComplete", false);
        profile.setPreference("browser.download.manager.closeWhenDone", false);
        profile.updatePreferences(drv.browser);
        var driver = basic.getDriver(drv);
        basic.openPage(driver, drv);

        //driver = new FirefoxDriver(profile);

        //basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

        var login = 'karpovrt', password = '123',  firstName = '2', lastName = 'Администратор2';
        driver.findElement({css:'input[id="login"]'}).sendKeys(login).thenCatch(function (e) {
            errrorHandlerFunction(e, "****** PHASE#1 : ERROR = Cannot input login")});
        driver.findElement({css:'input[id="password"]'}).sendKeys(password).thenCatch(function (e) {
            errrorHandlerFunction(e, "****** PHASE#1 : ERROR = Cannot input password")});
        driver.findElement({css:'button[id="submit"]'}).click().thenCatch(function (e) {
            errrorHandlerFunction(e, "****** PHASE#1 : ERROR = Cannot submit login/password")});
        driver.findElement({css:'button[id="submit"]'}).sendKeys(webdriver.Key.ENTER).thenCatch(function (e) {})
            .thenCatch(function (e) {errrorHandlerFunction(e, "****** PHASE#1 : ERROR = Cannot press enter")});
        driver.sleep(basic.EXTRA_SLOW_OPERATION * 2);
        driver.wait
        (
            webdriver.until.elementIsVisible(driver.findElement({id:'user-info'})),
            basic.FAST_OPERATION
        ).thenCatch(function (e) {errrorHandlerFunction(e, "****** PHASE#1 : ERROR = Login:Seems 'user-info' is not visible");});
        driver.wait
        (
            webdriver.until.elementTextContains(driver.findElement({id:'user-info'}), firstName),
            basic.FAST_OPERATION
        ).thenCatch(function (e) {errrorHandlerFunction(e, "****** PHASE#1 : ERROR = Login:Cannot find user first name")});
        driver.wait
        (
            webdriver.until.elementTextContains(driver.findElement({id:'user-info'}), lastName),
            basic.FAST_OPERATION
        ).thenCatch(function (e) {errrorHandlerFunction(e, "****** PHASE#1 : ERROR = Login:Cannot find user last name")});

        //PHASE#1: Download file
        basic.execute(driver, 'click', '#user-info', "****** PHASE#1 : ERROR = Cannot click on 'user-info' button");
        driver.sleep(basic.SLOW_OPERATION);
        basic.execute(driver, 'click', 'a[href="#/td:RomanKarpov//v-ui:Graph"]', "****** PHASE#1 : ERROR = Cannot click on 'glyphicon-link'");
        driver.sleep(basic.SLOW_OPERATION);
        basic.execute(driver, 'click', 'button[id="export-ttl"]', "****** PHASE#1 : ERROR = Cannot click on 'export-ttl' button");
        driver.sleep(basic.FAST_OPERATION);

        //PHASE#2: Upload file
        //var filePath = 'C:\\Users\\Administrator\\Downloads\\exported_graph.ttl';
        var filePath = "C:\\Users\\zugzug\\Downloads\\exported_graph.ttl";

        driver.findElement({id:'menu'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#2 : ERROR = Cannot click on settings button")});
        driver.wait
        (
            webdriver.until.elementIsVisible(driver.findElement({css:'li[id="menu"] li[resource="v-l:Create"]'})),
            basic.FAST_OPERATION
        ).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#2 : ERROR = Seems there is no `create` button inside menu")});

        driver.findElement({css:'li[id="menu"] li[resource="v-l:Create"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#2 : ERROR = Cannot click on `create` button")});
        driver.wait
        (
            webdriver.until.elementIsVisible(driver.findElement({css:'div[resource="v-fc:Create"]'})),
            basic.FAST_OPERATION
        ).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#2 : ERROR = Create template was not opened")});
        driver.findElement({css:'.fulltext'}).clear();
        driver.findElement({css:'.fulltext'}).sendKeys('Отчет')
            .thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#2 : ERROR = Cannot enter template name")});
        driver.sleep(basic.FAST_OPERATION);
        driver.wait
        (
            function () {
                return driver.findElements({css:'veda-control.fulltext div.tt-suggestion>p'}).then(function (suggestions) {
                    return webdriver.promise.filter(suggestions, function(suggestion) {
                        return suggestion.getText().then(function(txt){ return txt == 'Отчет' });
                    }).then(function(x) { return x.length>0; });
                });
             },
            basic.SLOW_OPERATION
        ).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#2 : ERROR = Dropdown doesnt contains value `Отчет`")});
        driver.findElements({css:'veda-control.fulltext div.tt-suggestion>p'}).then(function (suggestions) {
            webdriver.promise.filter(suggestions, function(suggestion) {
                return suggestion.getText().then(function(txt){ return txt == 'Отчет' });
            }).then(function(x) { driver.executeScript("document.elementFromPoint(800, 300).click();");});
        }).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#2 : ERROR = Cannot click on `Отчет` from dropdown")});
        driver.sleep(basic.SLOW_OPERATION);
        driver.wait
        (
            webdriver.until.elementIsVisible(driver.findElement({css:'span[about="v-s:Report"]'})),
            basic.FAST_OPERATION
        ).thenCatch(function (e) {basic.errorHandler(e, "****** PHASE#2 : ERROR = Seems that create operation not works properly")});
        driver.executeScript("document.querySelector('strong[about=\"v-s:attachment\"]').scrollIntoView(true);")
            .thenCatch(function(e){basic.errorHandler(e, "****** PHASE#2 : ERROR = Cannot scroll to 'v-s:attachment' field")});
        basic.execute(driver, 'sendKeys', 'input[type="file"]', "Cannot find '" + filePath + "' file", filePath);
        driver.sleep(basic.FAST_OPERATION);
        basic.execute(driver, 'click', 'span[property="v-s:fileName"]', "Cannot click on file name");
    }
    if (drv.browser == 'internet explorer') {
        var driver = basic.getDriver(drv);
        basic.openPage(driver, drv);
        driver.sleep(basic.SLOW_OPERATION);
        basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

        basic.execute(driver, 'click', '#user-info', "Cannot click on 'user-info' button");
        basic.execute(driver, 'click', 'a[href="#/td:RomanKarpov//v-ui:Graph"]', "Cannot click on 'glyphicon-link'");
        driver.sleep(basic.FAST_OPERATION);
        basic.execute(driver, 'click', 'button[id="export-ttl"]', "Cannot click on 'export-ttl' button");


        var filePath = 'C:\\Users\\Administrator\\Downloads\\exported_graph.ttl';
        //var filePath = "C:\\Users\\zugzug\\Downloads\\exported_graph.ttl";
        basic.openCreateDocumentForm(driver, 'Отчет', 'v-s:Report');
        driver.executeScript("document.querySelector('strong[about=\"v-s:attachment\"]').scrollIntoView(true);");
        basic.execute(driver, 'sendKeys', 'input[type="file"]', "Cannot find '" + filePath + "' file", filePath);

        basic.execute(driver, 'click', 'span[property="v-s:fileName"]', "Cannot click on file name");
        //driver.switchTo().alert().accept();
    }

    driver.quit();
});

