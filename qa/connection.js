var webdriver = require('selenium-webdriver');
module.exports = {
// FOR SAUCELABS TESTING
    driver :  new webdriver.Builder().usingServer('http://localhost:4445/wd/hub').withCapabilities({
                                                                          browserName: 'chrome',
                                                                          'tunnel-identifier' : process.env.TRAVIS_JOB_NUMBER,
                                                                          build : process.env.TRAVIS_BUILD_NUMBER,
                                                                          username: process.env.SAUCE_USERNAME,
                                                                          accessKey: process.env.SAUCE_ACCESS_KEY
                                                                         }).build()
// FOR LOCAL TESTING
/*
   driver : new webdriver.Builder().withCapabilities(webdriver.Capabilities.chrome()).build()
*/
}
