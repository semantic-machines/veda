var webdriver = require('selenium-webdriver');

function getDriver() {
	if (process.env.TRAVIS_BUILD_NUMBER === undefined) {
		// for local testing in chrome
		return new webdriver.Builder().withCapabilities(webdriver.Capabilities.chrome()).build() 
	} else {
		// for SauceLabs testing 
		return new webdriver.Builder().usingServer('http://localhost:4445/wd/hub').withCapabilities({
                                                                      browserName: 'chrome',
                                                                      'tunnel-identifier' : process.env.TRAVIS_JOB_NUMBER,
                                                                      build : process.env.TRAVIS_BUILD_NUMBER,
                                                                      username: process.env.SAUCE_USERNAME,
                                                                      accessKey: process.env.SAUCE_ACCESS_KEY
                                                                     }).build()
	}
}                                                                         

module.exports = {
	driver : getDriver(),	
}
