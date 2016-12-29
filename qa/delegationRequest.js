var webdriver = require('selenium-webdriver'),
    basic = require('./basic.js');

function findUp(driver) {
    return driver.findElements({css:'.glyphicon-chevron-up'}).then(function (result) {
        return result[1];
    })
}

function clickUp(element) {
    element.click()
        .thenCatch(function (e) {basic.errorHandler(e,"Cannot click on 2nd glyphicon-chevron-up");});
}


module.exports = {
    createRequestDelegation: function (driver, valueToSearch, valueToChoose ) {
        basic.openCreateDocumentForm(driver, 'Заявка на делегирование для пользователя', 'v-s:RequestDelegationUser');

        driver.executeScript("document.querySelector('#positions').scrollIntoView(true);");
        driver.findElement({css:'div[id="positions"] input[id="td:Analyst1"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'Аналитик' position")})

        driver.findElement({css:'veda-control[rel="v-s:delegate"] input[id="fulltext"]'}).sendKeys(valueToSearch)
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot find attribute rel=v-s:delegate ");});
        driver.sleep(basic.FAST_OPERATION);
        driver.wait
        (
            function () {
                return driver.findElements({css:'veda-control[rel="v-s:delegate"] span[class="tt-dropdown-menu"] div[class="tt-dataset-dataset"] p'}).then(function (suggestions) {
                    return webdriver.promise.filter(suggestions, function(suggestion) {
                        return suggestion.getText().then(function(txt){
                            return txt.toLowerCase() === valueToChoose.toLowerCase();
                        });
                    }).then(function(x) { return x.length>0; });
                });
            },
            basic.SLOW_OPERATION
        ).thenCatch(function (e) {basic.errorHandler(e, "Cannot find '"+ valueToSearch +"' from dropdown");});

        driver.findElements({css:'veda-control[rel="v-s:delegate"] span[class="tt-dropdown-menu"] div[class="tt-dataset-dataset"] p'}).then(function (suggestions) {
            webdriver.promise.filter(suggestions, function(suggestion) {
                return suggestion.getText().then(function(txt){
                    if (valueToChoose === undefined) {
                        return txt.toLowerCase() == valueToSearch.toLowerCase();
                    } else {
                        return txt.toLowerCase() == valueToChoose.toLowerCase();
                    }
                });
            }).then(function(x) { x[0].click();});
        }).thenCatch(function (e) {basic.errorHandler(e, "Cannot click on '"+ valueToChoose +"' from dropdown");});

        driver.findElement({css:'veda-control[property="v-s:dateFrom"] input[type="text"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'dateFrom' input");});
        driver.findElement({css:'veda-control[property="v-s:dateTo"] input[type="text"]'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'dateFrom' input");});
        driver.wait(findUp(driver), basic.FAST_OPERATION).then(clickUp);
        driver.findElement({css:'veda-control[rel="v-s:delegate"] input[id="fulltext"]'}).click();
        driver.executeScript("document.querySelector('#save').scrollIntoView(true);");
        driver.findElement({css:'#save'}).click()
            .thenCatch(function (e) {basic.errorHandler(e, "Cannot click on 'save' button")})

    }
};