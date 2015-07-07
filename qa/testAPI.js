var connection = require('./connection.js'),
    driver = connection.driver,
    basic = require('./basic.js'),
    until = require('selenium-webdriver').until,
    By = require('selenium-webdriver').By;



driver.get('http://127.0.0.1:8080/tests');

driver.wait
(
  new until.Condition('all API tests execution', function compareCounts(wd) 
  {
    var count1 = 0;
    var count2 = -1;
    driver.findElements(By.css("#qunit-tests>li")).then(
     function(elements)
     {
       driver.findElements(By.css("#qunit-tests>.pass")).then(
         function(elements2)
         {
	   count1 = elements.length;
	   count2 = elements2.length;
           console.log(elements.length+' = '+elements2.length);
         })
     });
    return count1==count2;
  })
  ,
  60000
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