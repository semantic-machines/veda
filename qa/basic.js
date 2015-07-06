var By = require('selenium-webdriver').By;
 
module.exports = {
	openPage: function (driver) {
		driver.get('http://127.0.0.1:8080/');
	},
	login: function (driver) {
		driver.findElement(By.id('login')).sendKeys('karpovr');
		driver.findElement(By.id('password')).sendKeys('123');
		driver.findElement(By.id('submit')).click();
	}
};