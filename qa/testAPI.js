var connection = require('./connection.js'),
    driver = connection.driver,
    basic = require('./basic.js'),
    until = require('selenium-webdriver').until,
    By = require('selenium-webdriver').By;



driver.get('http://127.0.0.1:8080/tests');

driver.wait
(
  new until.Condition('all API tests execution', function () 
  {
    return driver.findElements(By.css("#qunit-tests>li")).then(
     function(elements)
     {
       return driver.findElements(By.css("#qunit-tests>.pass")).then(
         function(elements2)
         {
           return (elements.length == elements2.length);
         })
     });
  })
  ,
  90000
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