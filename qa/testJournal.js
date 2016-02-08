var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js'),
    person = require('./person.js'),
    assert = require('assert'),
    timeStamp = ''+Math.round(+new Date()/1000);

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
		
	assertCounts(driver, 1, 1, 0);
	
	// Update individual
	
	// Check Journal (+1 new version)
	
	// Go to draft
	
	// Check Journal (no changes)
	
	// Update individual
	
	// Check Journal (+1 new version)
	
	driver.quit();	
});
