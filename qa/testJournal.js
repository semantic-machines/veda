var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    person = require('./person.js'),
    assert = require('assert'),
    timeStamp = ''+Math.round(new Date()/1000);

function assertCounts(driver, totalCount, createCount, updateCount) {
	driver.findElements({css:'div.journal-record'}).then(function (result) {
		assert.equal(totalCount, result.length);
	}).thenCatch(function (e) {basic.errorHandler(e, "Invalid `total` journal elements count")});
	driver.findElements({css:'div.journal-record[typeof="v-s:DocumentCreated"]'}).then(function (result) {
		assert.equal(createCount, result.length);
	}).thenCatch(function (e) {basic.errorHandler(e, "Invalid `create` journal elements count")});
	driver.findElements({css:'div.journal-record[typeof="v-s:DocumentUpdated"]'}).then(function (result) {
		assert.equal(updateCount, result.length);
	}).thenCatch(function (e) {basic.errorHandler(e, "Invalid `update` journal elements count")});
}

basic.getDrivers().forEach (function (drv) {
	var driver = basic.getDriver(drv);
	basic.openPage(driver, drv);

//	basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');
	basic.login(driver, 'bychinat', '123', 'Андрей', 'Бычин');
	
	// Create v-s:Versioned individual
	
	person.createPerson(driver, drv, timeStamp);
	
	// Check Journal (+1 new version)	
	//TODO change this to "View journal" button click
	driver.findElement({css:'div[id="main"] > [typeof="v-s:Person"]'}).getAttribute('resource').then(function (individualId) {
		basic.openPage(driver, drv, '#/'+individualId+'j');
	}).thenCatch(function (e) {basic.errorHandler(e, "Seems person is not saved")});
	
	driver.sleep(basic.SLOW_OPERATION);
	
	assertCounts(driver, 1, 1, 0);
/*	
	// Update individual	
	// 		Return to document
	driver.findElement({css:'[rel="v-s:onDocument"] [typeof="v-s:Person"] a'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click to return on main document")});
	//		Click edit
	driver.findElement({css:'#main > div > div > div.panel-footer > #edit'}).click()
		.thenCatch(function (e) {basic.errorHandler(e, "Cannot click on `Edit` button")});
	//		Change something
	var somethingUnique = ''+Math.round(new Date()/1000);
	driver.findElement({css:'div[id="object"] [property="rdfs:label"] + veda-control input'}).sendKeys("Вася Пупкин "+somethingUnique).thenCatch(function (e) {basic.errorHandler(e, "Cannot fill rdfs:label for preson")});
	driver.findElement({css:'[property="v-s:middleName"] + veda-control input'}).sendKeys(somethingUnique).thenCatch(function (e) {basic.errorHandler(e, "Cannot fill v-s:middleName for preson")});
*/	
	// Check Journal (+1 new version)
	
	// Go to draft
	
	// Check Journal (no changes)
	
	// Update individual
	
	// Check Journal (+1 new version)
	
	driver.quit();	
});
