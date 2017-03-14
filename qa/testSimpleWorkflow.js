var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
	createNet = require('./createNet.js'),
    timeStamp = ''+Math.round(+new Date()/1000),
    assert = require('assert'),
	startForm = require('./startForm.js');

/**
 * Нажатие на кнопку
 * @param driver
 * @param button - кнопка
 * @param doctype - тип документа
*/

function clickButton(driver, button, doctype) {
	driver.executeScript("document.querySelector('button[id="+button+"]').scrollIntoView(true);");
	driver.wait
	(
		webdriver.until.elementIsEnabled(driver.findElement({css:'[typeof="'+doctype+'"] button[id="'+ button +'"]'})),
		basic.SLOW_OPERATION
	).thenCatch(function (e) {basic.errorHandler(e, "Cannot find " + button + " button");});
	basic.execute(driver, 'click', '[typeof="'+doctype+'"] button[id="'+ button +'"]', "Cannot click on "  + button +  " button");
}

function put(driver, type, text) {
	basic.execute(driver, 'sendKeys', 'div[property="' + type + '"]+veda-control[data-type="multilingualText"] textarea[class="form-control"]',
        "Cannot fill "+ type +" field", text);
}

/**
 * 1.Open page -> login(as karpovrt);
 * 2.Create Rule1 -> Save Rule1 -> Create Transformation1 -> Save Tranformation1;
 * 3.Create Net -> Create Task with executor, Rule1 and Transformation -> Connect net -> Save net -> Check net is working;
 * 4.Logout -> login(as bychinat) -> Check messages(1);
 * 5.Quit;
 *
 * 1.Открываем страницу -> Входим в систему под karpovrt;
 * 2.Создаем Правило1 -> Сохраняем Правило1 -> Создаем трансформацию1 -> Сохраняем трансформацию1;
 * 3.Создаем сеть -> Создаем задачу с исполнителем, правилом1 и трансформацией1 -> Соединяем сеть -> Сохраняем сеть ->
 * -> Проверяем, что сеть работает;
 * 4.Выходим из системы -> Входим в систему под bychinat -> Проверяем сообщения(1);
 * 5.Выход;
*/
basic.getDrivers().forEach (function (drv) {
	var driver = basic.getDriver(drv);
	basic.openPage(driver, drv);
	basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

	basic.openCreateDocumentForm(driver, 'Правило', 'v-wf:Rule');
	driver.executeScript("document.querySelector('div[property=\"rdfs:label\"]').scrollIntoView(true);");
	basic.execute(driver, 'sendKeys', 'div[property="rdfs:label"]+veda-control[data-type="multilingualString"] input[type="text"]',
        "Cannot fill 'rdfs:label' field", timeStamp);
	driver.executeScript("document.querySelector('div[property=\"v-wf:segregateElement\"]').scrollIntoView(true);");
	put(driver, 'v-wf:segregateElement', "contentName('@')");
	driver.executeScript("document.querySelector('div[property=\"v-wf:aggregate\"]').scrollIntoView(true);");
	put(driver, 'v-wf:aggregate', "putUri ('rdf:type', 'v-wf:DecisionForm');");
	put(driver, 'v-wf:aggregate', "putUri ('rdf:type', 'mnd-wf:UserTaskForm');");
	put(driver, 'v-wf:aggregate', "putString ('rdfs:label', 'задание');");
	put(driver, 'v-wf:aggregate', "putBoolean ('v-wf:isCompleted', false);");
	put(driver, 'v-wf:aggregate', "putExecutor ('v-wf:to');");
	put(driver, 'v-wf:aggregate', "putWorkOrder ('v-wf:onWorkOrder');");
	put(driver, 'v-wf:aggregate', "putUri ('v-wf:possibleDecisionClass', 'v-wf:DecisionAchieved');");
	clickButton(driver, "save", "v-wf:Rule");
	//driver.sleep(basic.FAST_OPERATION);
	basic.openCreateDocumentForm(driver, 'Трансформация', 'v-wf:Transform');
	driver.executeScript("document.querySelector('div[property=\"rdfs:label\"]').scrollIntoView(true);");
	basic.execute(driver, 'sendKeys', 'div[property="rdfs:label"]+veda-control[data-type="multilingualString"] input[type="text"]',
        "Cannot fill 'rdfs:label' field", timeStamp + 1);
	driver.executeScript("document.querySelector('strong[about=\"v-wf:transformRule\"]').scrollIntoView(true);");
	basic.chooseFromDropdown(driver, 'v-wf:transformRule', timeStamp, timeStamp);
	clickButton(driver, "save", "v-wf:Transform");

	createNet.startNet(driver, timeStamp);
	basic.execute(driver, 'click', '.create-task', "Cannot click on 'create-task' button");
	basic.execute(driver, 'click', '.state-task', "Cannot click on 'state-task' button");
	driver.executeScript("document.querySelector('span[about=\"v-wf:startDecisionTransform\"]').scrollIntoView(true);");
	basic.execute(driver, 'click', 'span[about="v-wf:startDecisionTransform"]', "Cannot click on 'v-wf:startDecisionTransform' field ");
	basic.execute(driver, 'click', 'veda-control[class="VCstartDecisionTransform fulltext dropdown create properties-editor"]',
        "Cannot click on 'VCstartDecisionTransform' field ");
	createNet.chooseFromDropdown(driver, 'VCstartDecisionTransform', timeStamp + 1, timeStamp + 1);
	driver.executeScript("document.querySelector('span[about=\"v-wf:executor\"]').scrollIntoView(true);");
	basic.execute(driver, 'click', 'span[about="v-wf:executor"]', "Cannot click on 'v-wf:executor' field ");
	basic.execute(driver, 'click', 'veda-control[class="VCexecutor fulltext dropdown create properties-editor"]', "Cannot click on 'VCexecutor' field ");
	createNet.chooseFromDropdown(driver, 'VCexecutor', 'Администратор4', 'Администратор4 : Аналитик');
	createNet.connectNet(driver, 'true');
	createNet.saveNet(driver);
	createNet.checkNet(driver, timeStamp, 'red', 'red', '-');

	basic.logout(driver);
	basic.login(driver, 'bychinat', '123', '4', 'Администратор4');
	driver.findElement({css:'li[about="v-ft:Inbox"] span[id=counter]'}).getText().then(function (result) {
        assert.equal(1, result.length);
    }).thenCatch(function (e) {basic.errorHandler(e, "Invalid `message` elements count");});

	driver.quit();
});
