var connection = require('./connection.js'),
    driver = connection.driver,
    basic = require('./basic.js'),
    until = require('selenium-webdriver').until,
    By = require('selenium-webdriver').By;

basic.openPage(driver);
basic.login(driver, 'karpovr', '123');

driver.wait
(
  until.elementTextContains(driver.findElement(By.id('current-user')),'Роман Карпов'),
  2000
).then
(
  null,
  function(err)
  {
    console.trace(err);
    process.exit(1);
  }
);
driver.quit();