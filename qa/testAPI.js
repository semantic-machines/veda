var connection = require('./connection.js'),
    driver = connection.driver,
    basic = require('./basic.js'),
    until = require('selenium-webdriver').until,
    By = require('selenium-webdriver').By;

function compareCounts() {
  driver.findElements(By.css("#qunit-tests>li")).then(
     function(elements)
     {
       driver.findElements(By.css(".pass")).then(
         function(elements2)
         {
           return elements.length==elements2.length;
         })
     });
  return false;
}

driver.get('http://127.0.0.1:8080/tests');

console.log(driver.findElements(By.css("#qunit-tests>li")));

driver.wait
(
  compareCounts(),
  1000
).then
(
  null,
  function(err)
  {
    console.trace(err);
  }
);
driver.quit();