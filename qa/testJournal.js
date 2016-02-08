var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    person = require('./person.js'),
    assert = require('assert');

function assertCounts(driver, drv, totalCount, createCount, updateCount) {
	// 		Go to journal
	driver.findElement({css:'div[id="main"] > [typeof="v-s:Person"]'}).getAttribute('resource').then(function (individualId) {
		driver.sleep(basic.SLOW_OPERATION); // wait
		driver.executeScript("document.querySelector('#journal').scrollIntoView(true);");
		driver.findElement({css:'#journal'}).click()
			.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `View Journal` button")});
	}).thenCatch(function (e) {basic.errorHandler(e, "Seems person is not saved")});	

	driver.findElements({css:'div.journal-record'}).then(function (result) {
		assert.equal(totalCount, result.length);
	}).thenCatch(function (e) {basic.errorHandler(e, "Invalid `total` journal elements count")});
	driver.findElements({css:'div.journal-record[typeof="v-s:DocumentCreated"]'}).then(function (result) {
		assert.equal(createCount, result.length);
	}).thenCatch(function (e) {basic.errorHandler(e, "Invalid `create` journal elements count")});
	driver.findElements({css:'div.journal-record[typeof="v-s:DocumentUpdated"]'}).then(function (result) {
		assert.equal(updateCount, result.length);
	}).thenCatch(function (e) {basic.errorHandler(e, "Invalid `update` journal elements count")});
	// 		Return to document
	driver.findElement({css:'[rel="v-s:onDocument"] [typeof="v-s:Person"] a'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click to return on main document")});
}

basic.getDrivers().forEach (function (drv) {
	var driver = basic.getDriver(drv);
	basic.openPage(driver, drv);

	basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');
//	basic.login(driver, 'bychinat', '123', 'Андрей', 'Бычин');
	
	person.createPerson(driver, drv, 'first');
	
	// Check Journal (+1 new version)	
	assertCounts(driver, drv, 1, 1, 0);
	
	// Update individual	
	//		Click edit
	driver.executeScript("document.querySelector('#main > div > div > div.panel-footer > #edit').scrollIntoView(true);");
	driver.findElement({css:'#main > div > div > div.panel-footer > #edit'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Edit` button")});
	//		Change something
	driver.executeScript("document.querySelector('[property=\"v-s:middleName\"] + veda-control input').scrollIntoView(true);");
	driver.findElement({css:'[property="v-s:middleName"] .glyphicon-remove'}).click().thenCatch(function (e) {basic.errorHandler(e, "Cannot remove old v-s:middleName value")});
	driver.findElement({css:'[property="v-s:middleName"] + veda-control input'}).sendKeys('second').thenCatch(function (e) {basic.errorHandler(e, "Cannot fill v-s:middleName for preson")});
	driver.findElement({css:'[property="v-s:firstName"] + veda-control input'}).click().thenCatch(function (e) {basic.errorHandler(e, "Cannot fill v-s:middleName for preson")});
	//		Click save
	driver.executeScript("document.querySelector('#main > div > div > div.panel-footer > #save').scrollIntoView(true);");
	driver.findElement({css:'#main > div > div > div.panel-footer > #save'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Save` button")});
	
	// Check Journal (+1 new version)
	assertCounts(driver, drv, 2, 1, 1);
	
	// Update nothing
	//		Click edit
	driver.executeScript("document.querySelector('#main > div > div > div.panel-footer > #edit').scrollIntoView(true);");
	driver.findElement({css:'#main > div > div > div.panel-footer > #edit'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Edit` button")});
	//		Click save
	driver.executeScript("document.querySelector('#main > div > div > div.panel-footer > #save').scrollIntoView(true);");
	driver.findElement({css:'#main > div > div > div.panel-footer > #save'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Save` button")});
	
	// Check Journal (no changes)
	assertCounts(driver, drv, 3, 1, 2);
	
	// Update individual	
	//		Click edit
	driver.executeScript("document.querySelector('#main > div > div > div.panel-footer > #edit').scrollIntoView(true);");
	driver.findElement({css:'#main > div > div > div.panel-footer > #edit'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Edit` button")});
	//		Change something
	driver.executeScript("document.querySelector('[property=\"v-s:middleName\"] + veda-control input').scrollIntoView(true);");
	driver.findElement({css:'[property="v-s:middleName"] .glyphicon-remove'}).click().thenCatch(function (e) {basic.errorHandler(e, "Cannot remove old v-s:middleName value")});
	driver.findElement({css:'[property="v-s:middleName"] + veda-control input'}).sendKeys('third').thenCatch(function (e) {basic.errorHandler(e, "Cannot fill v-s:middleName for preson")});
	driver.findElement({css:'[property="v-s:firstName"] + veda-control input'}).click().thenCatch(function (e) {basic.errorHandler(e, "Cannot fill v-s:middleName for preson")});
	//		Click save
	driver.executeScript("document.querySelector('#main > div > div > div.panel-footer > #save').scrollIntoView(true);");
	driver.findElement({css:'#main > div > div > div.panel-footer > #save'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Save` button")});
	
	// Check Journal (+1 new version)
	assertCounts(driver, drv, 4, 1, 3);
	
	driver.quit();	
});
