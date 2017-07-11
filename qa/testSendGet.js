var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js');
/**
 * For FF, IE, GC browsers:
 * 1.Open page -> login(as karpovrt);
 * 2.Download selected graph(file);
 * 3.Upload file;
 * 4.Download file -> Upload new file;
 * 5.Quit;
 *
 * 1.Открываем страницу -> Входим в систему под karpovrt;
 * 2.Скачиваем выбранный граф(файл);
 * 3.Загружаем файл на сервер;
 * 4.Загружаем файл с сервера -> Загружаем новый файл на сервер;
 * 5.Выход;
 */


basic.getThreeDrivers().forEach(function (drv) {
    if (drv.browser == 'chrome') {
        var driver = basic.getDriver(drv);
        basic.openPage(driver, drv);
        basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');
        //basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

        basic.execute(driver, 'click', '#user-info', "Cannot click on 'user-info' button");
        basic.execute(driver, 'click', 'a[href="#/td:RomanKarpov//v-ui:Graph"]', "Cannot click on 'glyphicon-link'");
        driver.sleep(basic.FAST_OPERATION);
        basic.execute(driver, 'click', 'button[id="export-ttl"]', "Cannot click on 'export-ttl' button");

        var filePath = 'C:/Users/Administrator/Downloads/exported_graph.ttl';
        //var filePath = 'C:/Users/zugzug/Downloads/exported_graph.ttl';
        basic.openCreateDocumentForm(driver, 'Отчет', 'v-s:Report');
        driver.executeScript("document.querySelector('strong[about=\"v-s:attachment\"]').scrollIntoView(true);");
        basic.execute(driver, 'sendKeys', 'input[type="file"]', "Cannot find '" + filePath + "' file", filePath);
        driver.sleep(basic.FAST_OPERATION);

        basic.execute(driver, 'click', 'span[property="v-s:fileName"]', "Cannot click on file name");
    }
    if (drv.browser == 'firefox') {
        var driver = basic.getDriver(drv);
        basic.openPage(driver, drv);
        driver.sleep(basic.SLOW_OPERATION);
        basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

        var filePath = 'C:\\Users\\Administrator\\Downloads\\exported_graph.ttl';
        //var filePath = "C:\\Users\\zugzug\\Downloads\\exported_graph.ttl";
        basic.openCreateDocumentForm(driver, 'Отчет', 'v-s:Report');
        driver.executeScript("document.querySelector('strong[about=\"v-s:attachment\"]').scrollIntoView(true);");
        basic.execute(driver, 'sendKeys', 'input[type="file"]', "Cannot find '" + filePath + "' file", filePath);

        basic.execute(driver, 'click', 'span[property="v-s:fileName"]', "Cannot click on file name");
    }
    if (drv.browser == 'internet explorer') {
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

