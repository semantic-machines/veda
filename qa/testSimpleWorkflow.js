var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
	createNet = require('./createNet.js'),
    timeStamp = ''+Math.round(+new Date()/1000),
	startForm = require('./startForm.js');

function clickButton(driver, button, doctype) {
	driver.executeScript("document.querySelector('button[id="+button+"]').scrollIntoView(true);");
	driver.wait
	(
		webdriver.until.elementIsEnabled(driver.findElement({css:'[typeof="'+doctype+'"] button[id="'+ button +'"]'})),
		basic.SLOW_OPERATION
	).thenCatch(function (e) {basic.errorHandler(e, "Cannot find " + button + " button");});
	driver.findElement({css:'[typeof="'+doctype+'"] button[id="'+ button +'"]'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on "  + button +  " button");});
}

function put(driver, type, text) {
	driver.findElement({css:'div[property="' + type + '"]+veda-control[data-type="multilingualText"] textarea[class="form-control"]'}).sendKeys(text)
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot fill "+ type +" field");});
}


basic.getDrivers().forEach (function (drv) {
	var driver = basic.getDriver(drv);

	basic.openPage(driver, drv);
	basic.login(driver, 'karpovrt', '123', '2', 'Администратор2');

	basic.openCreateDocumentForm(driver, 'Правило', 'v-wf:Rule');
	driver.executeScript("document.querySelector('div[property=\"rdfs:label\"]').scrollIntoView(true);");
	driver.findElement({css:'div[property="rdfs:label"]+veda-control[data-type="multilingualString"] input[type="text"]'}).sendKeys(timeStamp)
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'rdfs:label' field");});

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
	driver.findElement({css:'div[property="rdfs:label"]+veda-control[data-type="multilingualString"] input[type="text"]'}).sendKeys(timeStamp + 1)
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'rdfs:label' field");});
	driver.executeScript("document.querySelector('strong[about=\"v-wf:transformRule\"]').scrollIntoView(true);");
	basic.chooseFromDropdown(driver, 'v-wf:transformRule', timeStamp, timeStamp);
	clickButton(driver, "save", "v-wf:Transform");

	createNet.startNet(driver, timeStamp);
	driver.findElement({css:'.create-task'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'create-task' button");});
	driver.findElement({css:'.state-task'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'state-task' button");});
	driver.executeScript("document.querySelector('span[about=\"v-wf:startDecisionTransform\"]').scrollIntoView(true);");
	driver.findElement({css:'span[about="v-wf:startDecisionTransform"]'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'v-wf:startDecisionTransform' field ");});
	driver.findElement({css:'veda-control[class="VCstartDecisionTransform fulltext dropdown create properties-editor"]'}).click()
	 	.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'VCstartDecisionTransform' field ");});
	createNet.chooseFromDropdown(driver, 'VCstartDecisionTransform', timeStamp + 1, timeStamp + 1);
	driver.executeScript("document.querySelector('span[about=\"v-wf:executor\"]').scrollIntoView(true);");
	driver.findElement({css:'span[about="v-wf:executor"]'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'v-wf:executor' field ");});
	driver.findElement({css:'veda-control[class="VCexecutor fulltext dropdown create properties-editor"]'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'VCexecutor' field ");});
	createNet.chooseFromDropdown(driver, 'VCexecutor', 'Администратор4', 'Администратор4 : Аналитик');
	createNet.connectNet(driver, 'true');
	createNet.saveNet(driver);
	createNet.checkNet(driver, timeStamp, 'red', 'red', '-');
	basic.logout(driver);
	basic.login(driver, 'bychinat', '123', '4', 'Администратор4');

	driver.findElement({id:'menu'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button");});
	basic.isVisible(driver, 'li[id="menu"] li[resource="v-l:Inbox"]', basic.SLOW_OPERATION);
	driver.findElement({css:'li[id="menu"] li[resource="v-l:Inbox"]'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `exit` button");});
	var container = driver.findElement({id:'main'});
	var content = container.innerHTML;
	container.innerHTML = content;
	driver.findElement({css:'a[property="rdfs:label"]'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot find a task");});

	driver.quit();
});
