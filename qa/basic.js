/**
 * You can specify OS/browsers in `drivers` method
 */
var webdriver = require('selenium-webdriver'),
    FAST_OPERATION = 2000, 			// 2000ms  = 2sec  - time limit for fast operations 
	SLOW_OPERATION = 10000,			// 10000ms = 10sec - time limit for fast operations
	EXTRA_SLOW_OPERATION = 100000,	//  time limit for extra slow operations
	SERVER_ADDRESS = (process.env.TRAVIS_BUILD_NUMBER === undefined)?'http://veda:8080/':'http://127.0.0.1:8080/';

webdriver.promise.controlFlow().on('uncaughtException', function(e) {
	console.trace(e);
	process.exit(1);
});

module.exports = {
	FAST_OPERATION: FAST_OPERATION,
	SLOW_OPERATION: SLOW_OPERATION,
	EXTRA_SLOW_OPERATION: EXTRA_SLOW_OPERATION,
	getDrivers: function () {
		if (process.env.TRAVIS_BUILD_NUMBER === undefined) {
			return  [
//			         {'os':'Windows 7',		'browser':'firefox',				'version':'40.0'},
			         {'os':'Windows 7',		'browser':'chrome',					'version':'43.0'},
//					 {'os':'Windows 7',		'browser':'opera',					'version':'32.0'},
			         {'os':'Windows 7',		'browser':'internet explorer',		'version':'11.0'}
			        ];
		} else {
			return  [
//			       {'os':'Windows 10',		'browser':'chrome',					'version':'43.0'},
//			       {'os':'Windows 8.1',		'browser':'chrome',					'version':'43.0'},
			       {'os':'Windows 7',		'browser':'chrome',					'version':'43.0'},
//			       {'os':'Windows XP',		'browser':'chrome',					'version':'43.0'},
//			       {'os':'Linux',			'browser':'chrome',					'version':'43.0'},
//			       {'os':'Windows 10',		'browser':'internet explorer',		'version':'11.0'},
//			       {'os':'Windows 8.1',		'browser':'internet explorer',		'version':'11.0'},
//			       {'os':'Windows 7',		'browser':'internet explorer',		'version':'11.0'},
//			       {'os':'Windows 7',		'browser':'opera',					'version':'11.64'},
//			       {'os':'Windows XP',		'browser':'opera',					'version':'11.64'},
//			       {'os':'Linux',			'browser':'opera',					'version':'12.15'},
//			       {'os':'Windows 10',		'browser':'firefox',				'version':'40.0'},
//			       {'os':'Windows 8.1',		'browser':'firefox',				'version':'40.0'},
//			       {'os':'Windows 7',		'browser':'firefox',				'version':'40.0'},
//			       {'os':'Windows XP',		'browser':'firefox',				'version':'40.0'},
//			       {'os':'Linux',			'browser':'firefox',				'version':'40.0'}
			        ];
		}
	},
	getDriver: function (driver) {
		if (process.env.TRAVIS_BUILD_NUMBER === undefined) 
		{
			// for remote tetsing via local selenium
			if (process.env.LOCAL === undefined) 
			{
				return new webdriver.Builder().usingServer('http://selenium-local:4444/wd/hub').withCapabilities({
															platform: driver.os,
															browserName : driver.browser,
															version : driver.version,
															'screenResolution' : '1280x960',
															ignoreZoomSetting : true,
															proxy : {proxyType: 'direct'}
														 }).build();
			} else {
				// for local testing in chrome
				return new webdriver.Builder().withCapabilities(webdriver.Capabilities.chrome()).build(); 
			}
		} else {
			// for remote testing via Sauce Labs
			return new webdriver.Builder().usingServer('http://localhost:4445/wd/hub').withCapabilities({
								platform: driver.os,
				                browserName : driver.browser,
				                version : driver.version,
				                'screenResolution' : '1280x960',
				                'tunnel-identifier' : process.env.TRAVIS_JOB_NUMBER,
				                build : process.env.TRAVIS_BUILD_NUMBER,
				                username: process.env.SAUCE_USERNAME,
				                accessKey: process.env.SAUCE_ACCESS_KEY
				               }).build();
		}
	},
	openPage: function (driver, driverAbout, path) {
		if (path === undefined) {
			driver.get(SERVER_ADDRESS).then(function() {
				console.log('****** GET NEW PAGE. PLATFORM > '+driverAbout.os+' / '+driverAbout.browser+' / '+driverAbout.version);
			}); 
		} else {
			driver.get(SERVER_ADDRESS+path).then(function() {
				console.log('****** GET NEW PAGE. PLATFORM > '+driverAbout.os+' / '+driverAbout.browser+' / '+driverAbout.version);
			});
		}
		driver.manage().window().setSize(1280, 1024);
	},
	/**
	 * @param login - логин пользователя
	 * @param password - пароль пользователя
	 * @param assertUserFirstName - имя пользователя (для проверки коректности входа)
	 * @param assertUserLastName - фамилия пользователя (для проверки коректности входа)
	 */
	login: function (driver, login, password, assertUserFirstName, assertUserLastName) {
		// Вводим логин и пароль 
		driver.findElement({css:'input[id="login"]'}).sendKeys(login);
		driver.findElement({css:'input[id="password"]'}).sendKeys(password);
		driver.findElement({css:'button[id="submit"]'}).click();
		driver.findElement({css:'button[id="submit"]'}).sendKeys(webdriver.Key.ENTER).thenCatch(function (e) {});
		
		// Проверям что мы залогинены корректно
		driver.wait
		(
		  webdriver.until.elementTextContains(driver.findElement({id:'user-info'}), assertUserFirstName),
		  FAST_OPERATION
		);
		driver.wait
		(
		  webdriver.until.elementTextContains(driver.findElement({id:'user-info'}), assertUserLastName),
		  FAST_OPERATION
		);
	},
	/**
	 * Заполнить ссылочный атрибут значением из выпадающего списка 
	 * 
	 * @param atribute - rdf:type атрибута, который будет заполнятся выбором из выпадающего списка
	 * @param valueToSearch - значение, которое будет введено в строку поиска
	 * @param valueToChoose - значение, которое будет выбрано как результат поиска (без учёта регистра; если не указано, будет использовано значение value)
	 */
	chooseFromDropdown: function(driver, attribute, valueToSearch, valueToChoose) {
		driver.findElement({css:'div[rel="'+attribute+'"] + veda-control input[id="fulltext"]'}).sendKeys(valueToSearch);
		
		// Проверяем что запрашивамый объект появился в выпадающем списке
		driver.wait
		(
		  function () {
			  return driver.findElements({css:'div[rel="'+attribute+'"] + veda-control.fulltext div.tt-suggestion>p'}).then(function (suggestions) {
				  return webdriver.promise.filter(suggestions, function(suggestion) {
						return suggestion.getText().then(function(txt){return txt.toLowerCase() == valueToChoose.toLowerCase() });
				  }).then(function(x) { return x.length>0; });
			  });
		  },
		  FAST_OPERATION
		);		
		
		// Кликаем на запрашиваемый тип в выпавшем списке		
		driver.findElements({css:'div[rel="'+attribute+'"] + veda-control.fulltext div.tt-suggestion>p'}).then(function (suggestions) {
			
			webdriver.promise.filter(suggestions, function(suggestion) {				
				return suggestion.getText().then(function(txt){
					if (valueToChoose == undefined) {
						return txt.toLowerCase() == valueToSearch.toLowerCase();
					} else {
						return txt.toLowerCase() == valueToChoose.toLowerCase();
					}
				});
			}).then(function(x) { x[0].click();});
		});
	},
	/**
	 * Открыть форму создания документа определённого шаблона
	 * 
	 * @param templateName - имя шаблона, документ которого создается
	 * @param templateRdfType - rdf:type шаблона, документ которого создается
	 */
	openCreateDocumentForm: function (driver, templateName, templateRdfType) {
		// Клик `Документ` в главном меню
		driver.findElement({css:'li[resource="v-m:DocMenu"]'}).click();

		// Проверяем что открылось подменю 
		driver.wait
		(
		  webdriver.until.elementIsVisible(driver.findElement({css:'li[resource="v-m:Create"]'})),
		  FAST_OPERATION
		);

		// Клик `Создать`
		driver.findElement({css:'li[resource="v-m:Create"]'}).click();

		// Проверяем что открылась страница создания документов
		driver.wait
		(
		  webdriver.until.elementIsVisible(driver.findElement({css:'div[resource="v-fc:Create"]'})),
		  FAST_OPERATION
		);

		// Вводим запрашиваемый тип документа
		driver.findElement({id:'fulltext'}).sendKeys(templateName);

		// Проверяем что запрашиваемый тип появился в выпадающем списке
		driver.wait
		(
		  function () {
			  return driver.findElements({css:'veda-control.fulltext div.tt-suggestion>p'}).then(function (suggestions) {
				  return webdriver.promise.filter(suggestions, function(suggestion) {
						return suggestion.getText().then(function(txt){ return txt == templateName });
				  }).then(function(x) { return x.length>0; });
			  });
		  },
		  FAST_OPERATION
		);

		// Кликаем на запрашиваемый тип в выпавшем списке
		driver.findElements({css:'veda-control.fulltext div.tt-suggestion>p'}).then(function (suggestions) {
			webdriver.promise.filter(suggestions, function(suggestion) {
				return suggestion.getText().then(function(txt){ return txt == templateName });
			}).then(function(x) { x[0].click();});
		});
		
		// Проверяем что тип появился на экране
		driver.wait
		(
		  webdriver.until.elementIsVisible(driver.findElement({css:'span[about="'+templateRdfType+'"]'})),
		  FAST_OPERATION
		);
	},	
	/**
	 * Открыть форму полнотекстового поиска, при этом выбрать поиск внутри определённого шаблона
	 * 
	 * @param templateName - имя шаблона, документ которого ищется
	 * @param templateRdfType - rdf:type шаблона, документ которого ищется
	 */
	openFulltextSearchDocumentForm: function (driver, templateName, templateRdfType) {
		// Клик `Документ` в главном меню
		driver.findElement({css:'li[resource="v-m:DocMenu"]'}).click();

		// Проверяем что открылось подменю 
		driver.wait
		(
		  webdriver.until.elementIsVisible(driver.findElement({css:'li[resource="v-m:Find"]'})),
		  FAST_OPERATION
		);

		// Клик `Поиск`
		driver.findElement({css:'li[resource="v-m:Find"]'}).click();

		// Проверяем что открылась страница поиска
		driver.wait
		(
		  webdriver.until.elementIsVisible(driver.findElement({css:'div[resource="v-fs:Search"]'})),
		  FAST_OPERATION
		);

		// Вводим запрашиваемый тип документа
		driver.findElement({css:'div[typeof="v-fs:FulltextRequest"] input[id="fulltext"]'}).sendKeys(templateName);
		
		// Проверяем что запрашиваемый тип появился в выпадающем списке
		driver.wait
		(
		  function () {
			  return driver.findElements({css:'veda-control.fulltext div.tt-suggestion>p'}).then(function (suggestions) {
				  return webdriver.promise.filter(suggestions, function(suggestion) {
						return suggestion.getText().then(function(txt){ return txt == templateName });
				  }).then(function(x) { return x.length>0; });
			  });
		  },
		  FAST_OPERATION
		);
		
		// Кликаем на запрашиваемый тип в выпавшем списке
		driver.findElements({css:'veda-control.fulltext div.tt-suggestion>p'}).then(function (suggestions) {
			webdriver.promise.filter(suggestions, function(suggestion) {
				return suggestion.getText().then(function(txt){ return txt == templateName });
			}).then(function(x) { x[0].click();});
		});
		
		// Проверяем что тип появился на экране
		driver.wait
		(
		  webdriver.until.elementIsVisible(driver.findElement({css:'div[rel="v-fs:typeToSearch"] span[resource="'+templateRdfType+'"]'})),
		  FAST_OPERATION
		);
	}
};