var By = require('selenium-webdriver').By;
 
module.exports = {
	openPage: function (driver) {
		driver.get('http://127.0.0.1:8080/');
	},
	login: function (driver, login, password) {
		driver.findElement(By.id('login')).sendKeys(login);
		driver.findElement(By.id('password')).sendKeys(password);
		driver.findElement(By.id('submit')).click();
	}
};