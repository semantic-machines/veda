var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    person = require('./person.js'),
	timeStamp = ''+Math.round(+new Date()/1000);
    assert = require('assert');

function assertCounts(driver, totalCount, createCount, updateCount) {
	// 		Go to journal
	driver.wait
	(
	  function () {
		  driver.sleep(basic.SLOW_OPERATION);
		  return webdriver.until.elementLocated({css:'div.journal-record'});
	  },
	  basic.EXTRA_SLOW_OPERATION
	).thenCatch(function (e) {basic.errorHandler(e, "Cannot find action, after save operation");});
	driver.executeScript("document.querySelector('#journal').scrollIntoView(true);");
	driver.sleep(basic.SLOW_OPERATION).then(function() {
		driver.findElement({css:'#journal'}).click()
			.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `View Journal` button");});
	}).then(function() {
		driver.sleep(basic.FAST_OPERATION);
		var container = driver.findElement({id:'main'});
		var content = container.innerHTML;
		container.innerHTML = content;
		driver.findElements({css:'div.journal-record'}).then(function (result) {
			assert.equal(totalCount, result.length);
		}).thenCatch(function (e) {basic.errorHandler(e, "Invalid `total` journal elements count");});
		driver.findElements({css:'div.journal-record[typeof="v-s:DocumentCreated"]'}).then(function (result) {
			assert.equal(createCount, result.length);
		}).thenCatch(function (e) {basic.errorHandler(e, "Invalid `create` journal elements count");});
		driver.findElements({css:'div.journal-record[typeof="v-s:DocumentUpdated"]'}).then(function (result) {
			assert.equal(updateCount, result.length);
		}).thenCatch(function (e) {basic.errorHandler(e, "Invalid `update` journal elements count");});
		// 		Return to document
		driver.findElement({css:'[rel="v-s:onDocument"] [typeof="v-s:Action"] a'}).click()
			.thenCatch(function (e) {basic.errorHandler(e, "Cannot click to return on main document");});
	});
}


function update(driver, key) {
	driver.sleep(basic.FAST_OPERATION);
	driver.executeScript("document.querySelector('#edit').scrollIntoView(true);");
	driver.findElement({css:'#edit'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Edit` button");});
	driver.sleep(basic.FAST_OPERATION);
	if (key != '') {
		driver.executeScript("document.querySelector('strong[about=\"v-s:shortLabel\"]').scrollIntoView(true);");
		driver.findElement({css:'veda-control[property="v-s:shortLabel"] div[class="input-group"] textarea[class="form-control"]'}).clear();
		driver.findElement({css:'veda-control[property="v-s:shortLabel"] div[class="input-group"] textarea[class="form-control"]'}).sendKeys(key)
			.thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'v-s:shortLabel' field");});
		driver.sleep(basic.FAST_OPERATION);
	}
	driver.executeScript("document.querySelector('#save').scrollIntoView(true);");
	driver.findElement({id:'save'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Save` button");});
	driver.sleep(basic.FAST_OPERATION);
}


basic.getDrivers().forEach (function (drv) {
	var driver = basic.getDriver(drv);
	basic.openPage(driver, drv);

	basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');

	basic.openCreateDocumentForm(driver, 'Мероприятие', "v-s:Action");
	driver.executeScript("document.querySelector('div[property=\"rdfs:label\"]').scrollIntoView(true);");
	driver.findElement({css:'veda-control[property="rdfs:label"] div[class="input-group"] input[type="text"]'}).sendKeys(timeStamp)
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'rdfs:label' field");});
	driver.sleep(basic.FAST_OPERATION);
	driver.executeScript("document.querySelector('strong[about=\"v-s:shortLabel\"]').scrollIntoView(true);");
	driver.findElement({css:'veda-control[property="v-s:shortLabel"] div[class="input-group"] textarea[class="form-control"]'}).sendKeys(timeStamp + 1)
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot fill 'v-s:shortLabel' field");});
	driver.sleep(basic.FAST_OPERATION);
	driver.findElement({id:'save'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Save` button");});
	driver.sleep(basic.FAST_OPERATION);

	assertCounts(driver, 1, 1, 0);

	update(driver, timeStamp + 2);
	assertCounts(driver, 2, 1, 1);

	update(driver, '');
	assertCounts(driver, 3, 1, 2);

	update(driver, timeStamp + 3);
	assertCounts(driver, 4, 1, 3);

	driver.quit();	
});
