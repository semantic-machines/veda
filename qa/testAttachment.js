var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js');
/**
 * 0.Open page -> login(as karpovrt);
 * 1.Open Administrator graph -> Download ttl;
 * 2.Create Report -> Attach file -> Save;
 * 3.Download attached file -> Edit -> Delete file -> Attach file again -> Save;
 *
 * 0.Открываем страницу -> Входим в систему под karpovrt;
 * 1.Открывем граф Администратора -> Скачиваем ttl;
 * 2.Создаем Отчет -> Прикрепляем файл -> Сохраняем;
 * 3.Скачиваем прикрепленный файл -> Редактируем -> Убираем прикрепленный файл -> Прикрепляем заново файл -> Сохраняем;
*/


basic.getDrivers().forEach(function (drv) {
    //PHASE#0: Login
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2', 0);

    //PHASE#1: Download
    //basic.menu(driver, 'Graph');
    //basic.execute(driver, 'click', 'button[id="export-ttl"]', "Cannot click on 'export-ttl' button");
    basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person', 1);
    basic.execute(driver, 'click', 'button[id="submit"]', "****** PHASE#1 DOWNLOAD : ERROR = Cannot click on 'submit' button");
    driver.sleep(basic.SLOW_OPERATION);
    basic.execute(driver, 'click', 'a[href="#/cfg:Administrator"]', "****** PHASE#1 DOWNLOAD : ERROR = Cannot click on 'Администратор'");
    basic.execute(driver, 'click', 'a[href="#/cfg:Administrator//v-ui:Graph"]', "****** PHASE#1 DOWNLOAD : ERROR = Cannot click on 'glyphicon-link'");
    basic.execute(driver, 'click', 'button[id="export-ttl"]', "****** PHASE#1 DOWNLOAD : ERROR = Cannot click on 'export-ttl' button");

    //PHASE#2: Create -> Attach -> Save
    basic.openCreateDocumentForm(driver, 'Отчет', 'v-s:Report', 2);
    var filePath = 'C:/Users/Administrator/Downloads/exported_graph.ttl';
    //var filePath = 'C:/Users/zugzug/Downloads/exported_graph.ttl';
    driver.executeScript("document.querySelector('strong[about=\"v-s:attachment\"]').scrollIntoView(true);");
    basic.execute(driver, 'sendKeys', 'input[type="file"]', "****** PHASE#2 ATTACH : ERROR = Cannot find '" + filePath + "' file", filePath);
    driver.executeScript("document.querySelector('button[id=\"save\"]').scrollIntoView(true);");
    basic.execute(driver, 'click', 'button[id="save"]', "****** PHASE#2 SAVE : ERROR = Cannot click on 'save' button");

    //PHASE#3: Download -> Delete -> Attach -> Save
    driver.executeScript("document.querySelector('strong[about=\"v-s:attachment\"]').scrollIntoView(true);");
    basic.execute(driver, 'click', 'span[property="v-s:fileName"]', "****** PHASE#3 DOWNLOAD : ERROR = Cannot click on 'v-s:fileName'");
    driver.executeScript("document.querySelector('button[id=\"edit\"]').scrollIntoView(true);");
    basic.execute(driver, 'click', 'button[id="edit"]', "****** PHASE#3 EDIT : ERROR = Cannot click on 'edit' button");
    driver.executeScript("document.querySelector('div[rel=\"v-s:attachment\"]').scrollIntoView(true);");
    basic.isVisible(driver,'div[rel="v-s:attachment"] div+div[id="rel-actions"] .button-delete', basic.SLOW_OPERATION, 3);
    driver.executeScript("document.elementFromPoint(675, 20).click();");
    driver.executeScript("document.querySelector('strong[about=\"v-s:attachment\"]').scrollIntoView(true);");
    basic.execute(driver, 'sendKeys', 'input[type="file"]', "****** PHASE#3 ATTACH : ERROR = Cannot find this '" + filePath + "' file", filePath);
    basic.isVisible(driver, 'div[rel="v-s:attachment"]', basic.FAST_OPERATION);
    driver.executeScript("document.querySelector('button[id=\"save\"]').scrollIntoView(true);");
    basic.execute(driver, 'click', 'button[id="save"]', "****** PHASE#3 SAVE : ERROR = Cannot click on 'save' button");
    driver.quit();
});
