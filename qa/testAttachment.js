var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js');
/**
 * 1.Open page -> login(as karpovrt);
 * 2.Open Administrator graph -> download ttl;
 * 3.Create Report -> Attach file -> Save;
 * 4.Downlaod attached file -> Edit -> Delete file -> Attach file again -> Save;
 * 5.Quit;
 *
 * 1.Открываем страницу -> Входим в систему под karpovrt;
 * 2.Открывем граф Администратора -> Скачиваем ttl;
 * 3.Создаем Отчет -> Прикрепляем файл -> Сохраняем;
 * 4.Скачиваем прикрепленный файл -> Редактируем -> Убираем прикрепленный файл -> Прикрепляем заново файл -> Сохраняем;
 * 5.Выход;
*/


basic.getDrivers().forEach(function (drv) {
    var driver = basic.getDriver(drv);
    basic.openPage(driver, drv);
    basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

    //download
    //basic.menu(driver, 'Graph');
    //basic.execute(driver, 'click', 'button[id="export-ttl"]', "Cannot click on 'export-ttl' button");
    basic.openFulltextSearchDocumentForm(driver, 'Персона', 'v-s:Person');
    basic.execute(driver, 'click', 'button[id="submit"]', "Cannot click on 'submit' button");
    driver.sleep(basic.SLOW_OPERATION);
    basic.execute(driver, 'click', 'a[href="#/cfg:Administrator"]', "Cannot click on 'Администратор'");
    basic.execute(driver, 'click', 'a[href="#/cfg:Administrator//v-ui:Graph"]', "Cannot click on 'glyphicon-link'");
    basic.execute(driver, 'click', 'button[id="export-ttl"]', "Cannot click on 'export-ttl' button");

    //attach-save-check-delete-attach-save
    basic.openCreateDocumentForm(driver, 'Отчет', 'v-s:Report');
    var filePath = 'C:/Users/Administrator/Downloads/exported_graph.ttl';
    //var filePath = 'C:/Users/zugzug/Downloads/exported_graph.ttl';
    driver.executeScript("document.querySelector('strong[about=\"v-s:attachment\"]').scrollIntoView(true);");
    basic.execute(driver, 'sendKeys', 'input[type="file"]', "Cannot find this '" + filePath + "' file", filePath);
    driver.executeScript("document.querySelector('button[id=\"save\"]').scrollIntoView(true);");
    basic.execute(driver, 'click', 'button[id="save"]', "Cannot click on 'save' button");

    driver.executeScript("document.querySelector('strong[about=\"v-s:attachment\"]').scrollIntoView(true);");
    basic.execute(driver, 'click', 'span[property="v-s:fileName"]', "Cannot click on 'v-s:fileName'");
    driver.executeScript("document.querySelector('button[id=\"edit\"]').scrollIntoView(true);");
    basic.execute(driver, 'click', 'button[id="edit"]', "Cannot click on 'edit' button");
    driver.executeScript("document.querySelector('div[rel=\"v-s:attachment\"]').scrollIntoView(true);");
    basic.isVisible(driver,'div[rel="v-s:attachment"] div+div[id="rel-actions"] .button-delete', basic.SLOW_OPERATION);
    driver.executeScript("document.elementFromPoint(675, 20).click();");
    driver.executeScript("document.querySelector('strong[about=\"v-s:attachment\"]').scrollIntoView(true);");
    basic.execute(driver, 'sendKeys', 'input[type="file"]', "Cannot find this '" + filePath + "' file", filePath);
    basic.isVisible(driver, 'div[rel="v-s:attachment"]', basic.FAST_OPERATION);
    driver.executeScript("document.querySelector('button[id=\"save\"]').scrollIntoView(true);");
    basic.execute(driver, 'click', 'button[id="save"]', "Cannot click on 'save' button");
    
    driver.quit();
});
