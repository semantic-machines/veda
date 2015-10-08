var connection = require('./connection.js'),
    driver = connection.driver,
    basic = require('./basic.js');

basic.openPage(driver);

basic.login(driver, 'karpovrt', '123', 'Роман', 'Карпов');

driver.quit();