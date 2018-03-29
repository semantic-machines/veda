import { Selector, t } from 'testcafe';

export default class basic {
    async login(login, password) {
        await t
            .typeText('#login'	, login)
            .typeText('#password', password)
            .click('#submit');
    }
    
    async logout() {
		await t
			.click('#menu')
			.click('li[id="menu"] li[resource="v-l:Exit"]');
	}
    
    async openCreateDocumentForm(name) {
		await t
			.click('#menu')
			.click('li[id="menu"] li[resource="v-l:Create"]')
			.typeText('input.fulltext.tt-input', name)
			.click('veda-control.fulltext div.tt-suggestion>p')
	}
	
	async chooseFromDropdown(attribute, valueToSearch, valueToChoose) {
		
		
	}
	
	async createExample(name) {
		await t
			.click('#menu')
			.click('li[id="menu"] li[resource="v-l:Create"]')
			.typeText('#fulltext', 'Стартовая форма')
			.click('veda-control.fulltext div.tt-suggestion>p')
			
	}
	
	async openFulltextSearchDocumentForm(name) {
		await t
			.click('#menu')
			.click('li[id="menu"] li[resource="v-l:Find"]')
			.typeText('div[typeof="v-fs:FulltextRequest"] input.fulltext.tt-input', name)
			
	}
}

