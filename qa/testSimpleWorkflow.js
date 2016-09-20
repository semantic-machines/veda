var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    timeStamp = ''+Math.round(+new Date()/1000),
	startForm = require('./startForm.js');

function clickButton(driver, button) {
	driver.executeScript("document.querySelector('button[id="+button+"]').scrollIntoView(true);");
	driver.wait
	(
		webdriver.until.elementIsEnabled(driver.findElement({css:'button[id="'+ button +'"]'})),
		basic.SLOW_OPERATION
	).thenCatch(function (e) {basic.errorHandler(e, "Cannot find " + button + " button");});
	driver.sleep(basic.FAST_OPERATION);
	driver.findElement({css:'button[id="'+ button +'"]'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on "  + button +  " button");});
}

function put(driver, type, text) {
	driver.findElement({css:'div[property="' + type + '"]+veda-control[data-type="multilingualText"] textarea[class="form-control"]'}).sendKeys(text)
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot fill "+ type +" field");});
}


function choose(driver, type, valueToSearch, valueToChoose) {
	driver.findElement({css:'veda-control[class="'+ type +' fulltext dropdown create properties-editor"] input[id="fulltext"]'}).sendKeys(valueToSearch)
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot find attribute " + type + "");});
	driver.wait
	(
		function () {
			return driver.findElements({css:'veda-control[class="'+ type +' fulltext dropdown create properties-editor"] span[class="tt-dropdown-menu"] div[class="tt-dataset-dataset"] p'}).then(function (suggestions) {
				return webdriver.promise.filter(suggestions, function(suggestion) {
					return suggestion.getText().then(function(txt){
						return txt.toLowerCase() === valueToChoose.toLowerCase();
					});
				}).then(function(x) { return x.length>0; });
			});
		},
		basic.FAST_OPERATION
	).thenCatch(function (e) {basic.errorHandler(e, "Cannot find '"+ valueToSearch +"' from dropdown");});


	// Кликаем на запрашиваемый тип в выпавшем списке
	driver.findElements({css:'veda-control[class="'+ type +' fulltext dropdown create properties-editor"] span[class="tt-dropdown-menu"] div[class="tt-dataset-dataset"] p'}).then(function (suggestions) {
		webdriver.promise.filter(suggestions, function(suggestion) {
			return suggestion.getText().then(function(txt){
				if (valueToChoose === undefined) {
					return txt.toLowerCase() == valueToSearch.toLowerCase();
				} else {
					return txt.toLowerCase() == valueToChoose.toLowerCase();
				}
			});
		}).then(function(x) { x[0].click();});
	}).thenCatch(function (e) {basic.errorHandler(e, "Cannot click on '"+ valueToChoose +"' from dropdown");});
}

basic.getDrivers().forEach (function (drv) {
	var driver = basic.getDriver(drv);
	
	basic.openPage(driver, drv);
	basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');

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
	driver.executeScript("document.querySelector('button[id=\"save\"]').scrollIntoView(true);");
	driver.findElement({css:'button[id="save"]'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on save button");})
	driver.sleep(basic.SLOW_OPERATION);
	clickButton(driver, "save");


    driver.sleep(basic.SLOW_OPERATION);    
	basic.openCreateDocumentForm(driver, 'Трансформация', 'v-wf:Transform');
	driver.executeScript("document.querySelector('div[property=\"rdfs:label\"]').scrollIntoView(true);");
	driver.findElement({css:'div[property="rdfs:label"]+veda-control[data-type="multilingualString"] input[type="text"]'}).sendKeys(timeStamp + 1)
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'rdfs:label' field");});
	driver.executeScript("document.querySelector('strong[about=\"v-wf:transformRule\"]').scrollIntoView(true);");
	basic.chooseFromDropdown(driver, 'v-wf:transformRule', timeStamp, timeStamp);
	clickButton(driver, "save");

	basic.openCreateDocumentForm(driver, 'Сеть', 'v-wf:Net');
	driver.sleep(basic.FAST_OPERATION);
	driver.findElement({css:'.workflow-canvas-wrapper'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
	driver.sleep(basic.FAST_OPERATION);
	driver.findElement({css:'#props-col [about="rdfs:label"]'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on route rdfs:label");});
	driver.findElement({css:'#VClabel input'}).sendKeys(timeStamp)
			.thenCatch(function (e) {basic.errorHandler(e, "Cannot fill rdfl:label in net properties");});
	driver.findElement({css:'.create-task'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click add task");});
	driver.findElement({css:'.state-task'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click 'state-task'");});

	driver.executeScript("document.querySelector('span[about=\"v-wf:startDecisionTransform\"]').scrollIntoView(true);");
	driver.findElement({css:'span[about="v-wf:startDecisionTransform"]'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click 'v-wf:startDecisionTransform' field ");});
	driver.findElement({css:'veda-control[class="VCstartDecisionTransform fulltext dropdown create properties-editor"]'}).click()
	 	.thenCatch(function (e) {basic.errorHandler(e, "Cannot click 'VCstartDecisionTransform' field ");});
	choose(driver, 'VCstartDecisionTransform', timeStamp + 1, timeStamp + 1);

	driver.executeScript("document.querySelector('span[about=\"v-wf:executor\"]').scrollIntoView(true);");
	driver.findElement({css:'span[about="v-wf:executor"]'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click 'v-wf:executor' field ");});
	driver.findElement({css:'veda-control[class="VCexecutor fulltext dropdown create properties-editor"]'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click 'VCexecutor' field ");});
	choose(driver, 'VCexecutor', 'Андрей Бычин', 'Андрей Бычин : Аналитик');

	new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-io-condition-input .ep'}), driver.findElement({css:'.state-task'})).perform();
	new webdriver.ActionSequence(driver).dragAndDrop(driver.findElement({css:'.state-task .ep'}), driver.findElement({css:'.state-io-condition-output'})).perform();
	driver.findElement({css:'#workflow-save-button'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click save net");});
	driver.sleep(basic.FAST_OPERATION);

	startForm.createStartForm(driver, timeStamp, 'Ожидает отправки');
	driver.findElement({css:'.workflow-canvas-wrapper'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on net canvas");});
	driver.findElement({css:'.state-io-condition-input[colored-to="red"]'})
		.thenCatch(function (e) {basic.errorHandler(e, "Seems 'input' button is not located/red");});
	driver.sleep(basic.FAST_OPERATION);
	driver.findElement({css:'.state-task[colored-to="red"]'})
		.thenCatch(function (e) {basic.errorHandler(e, "Seems 'state-task' button is not located/red");});
	driver.sleep(basic.FAST_OPERATION);
	driver.findElement({css:'.state-io-condition-output'})
		.thenCatch(function (e) {basic.errorHandler(e, "Seems 'output' button is not located/colored");});
	driver.sleep(basic.FAST_OPERATION);



	driver.findElement({id:'menu'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button");});
	driver.wait
	(
		webdriver.until.elementIsVisible(driver.findElement({css:'li[id="menu"] li[resource="v-l:Exit"]'})),
		basic.FAST_OPERATION
	).thenCatch(function (e) {basic.errorHandler(e, "Seems there is no `exit` button inside menu");});
	driver.findElement({css:'li[id="menu"] li[resource="v-l:Exit"]'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `exit` button");});
	driver.sleep(basic.FAST_OPERATION);
	driver.findElement({css:'input[id="login"]'}).clear()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot clear 'login' field");});
	driver.findElement({css:'input[id="password"]'}).clear()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot clear 'password' field");});

	basic.login(driver, 'bychinat', '123', 'Андрей', 'Бычин');

	driver.findElement({id:'menu'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on settings button");});
	driver.wait
	(
		webdriver.until.elementIsVisible(driver.findElement({css:'li[id="menu"] li[resource="v-l:Inbox"]'})),
		basic.FAST_OPERATION
	).thenCatch(function (e) {basic.errorHandler(e, "Seems there is no `exit` button inside menu");});
	driver.findElement({css:'li[id="menu"] li[resource="v-l:Inbox"]'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `exit` button");});
	driver.sleep(basic.FAST_OPERATION);

	driver.findElement({css:'a[property="rdfs:label"]'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot find a task");});

	driver.quit();
});
